package io.github.pauljamescleary.petstore
package infrastructure.endpoint

import cats.data.NonEmptyList
import cats.effect._
import io.circe.generic.auto._
import io.github.pauljamescleary.petstore.domain.pets._
import io.github.pauljamescleary.petstore.domain.users._
import io.github.pauljamescleary.petstore.infrastructure.repository.inmemory._
import org.http4s._
import org.http4s.circe._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.dsl._
import org.http4s.implicits._
import org.http4s.server.Router
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import tsec.mac.jca.HMACSHA256

class PetEndpointsSpec
    extends AnyFunSuite
    with Matchers
    with ScalaCheckPropertyChecks
    with PetStoreArbitraries
    with Http4sDsl[IO]
    with Http4sClientDsl[IO] {

  implicit val petEnc: EntityEncoder[IO, Pet] = jsonEncoderOf
  implicit val petDec: EntityDecoder[IO, Pet] = jsonOf

  implicit val listPetDec: EntityDecoder[IO, List[Pet]] = jsonOf

  def getTestResources(): (AuthTest[IO], HttpApp[IO], PetRepositoryInMemoryInterpreter[IO]) = {
    val userRepo = UserRepositoryInMemoryInterpreter[IO]()
    val petRepo = PetRepositoryInMemoryInterpreter[IO]()
    val petValidation = PetValidationInterpreter[IO](petRepo)
    val petService = PetService[IO](petRepo, petValidation)
    val auth = new AuthTest[IO](userRepo)
    val petEndpoint = PetEndpoints.endpoints[IO, HMACSHA256](petService, auth.securedRqHandler)
    val petRoutes = Router(("/pets", petEndpoint)).orNotFound
    (auth, petRoutes, petRepo)
  }

  test("create pet") {

    val (auth, petRoutes, _) = getTestResources()

    forAll { pet: Pet =>
      (for {
        request <- POST(pet, uri"/pets")
        response <- petRoutes.run(request)
      } yield {
        response.status shouldEqual Unauthorized
      }).unsafeRunSync
    }

    forAll { (pet: Pet, user: User) =>
      (for {
        request <- POST(pet, uri"/pets")
          .flatMap(auth.embedToken(user, _))
        response <- petRoutes.run(request)
      } yield {
        response.status shouldEqual Ok
      }).unsafeRunSync
    }

    forAll { (pet: Pet, user: User) =>
      (for {
        createRq <- POST(pet, uri"/pets")
          .flatMap(auth.embedToken(user, _))
        response <- petRoutes.run(createRq)
        createdPet <- response.as[Pet]
        getRq <- GET(Uri.unsafeFromString(s"/pets/${createdPet.id.get}"))
          .flatMap(auth.embedToken(user, _))
        response2 <- petRoutes.run(getRq)
      } yield {
        response.status shouldEqual Ok
        response2.status shouldEqual Ok
      }).unsafeRunSync
    }
  }

  test("update pet") {

    val (auth, petRoutes, _) = getTestResources()

    forAll { (pet: Pet, user: AdminUser) =>
      (for {
        createRequest <- POST(pet, uri"/pets")
          .flatMap(auth.embedToken(user.value, _))
        createResponse <- petRoutes.run(createRequest)
        createdPet <- createResponse.as[Pet]
        petToUpdate = createdPet.copy(name = createdPet.name.reverse)
        updateRequest <- PUT(petToUpdate, Uri.unsafeFromString(s"/pets/${petToUpdate.id.get}"))
          .flatMap(auth.embedToken(user.value, _))
        updateResponse <- petRoutes.run(updateRequest)
        updatedPet <- updateResponse.as[Pet]
      } yield {
        updatedPet.name shouldEqual pet.name.reverse
      }).unsafeRunSync
    }
  }

  test("find by tag") {

    val (auth, petRoutes, petRepo) = getTestResources()

    forAll { (pet: Pet, user: AdminUser) =>
      (for {
        createRequest <- POST(pet, uri"/pets")
          .flatMap(auth.embedToken(user.value, _))
        createResponse <- petRoutes.run(createRequest)
        createdPet <- createResponse.as[Pet]
      } yield {
        createdPet.tags.toList.headOption match {
          case Some(tag) =>
            val petsFoundByTag = petRepo.findByTag(NonEmptyList.of(tag)).unsafeRunSync
            petsFoundByTag.contains(createdPet) shouldEqual true
          case _ => ()
        }
      }).unsafeRunSync
    }

  }

  test("delete pet") {
    val (auth, petRoutes, petRepo) = getTestResources()

    forAll { (pet: Pet, user: AdminUser) =>
      (for {
        createRequest <- POST(pet, uri"/pets")
          .flatMap(auth.embedToken(user.value, _))
        createResponse <- petRoutes.run(createRequest)
        createPet <- createResponse.as[Pet]
        deleteRequest <- DELETE(Uri.unsafeFromString(s"/pets/${createPet.id.get}"))
          .flatMap(auth.embedToken(user.value, _))
        _ <- petRoutes.run(deleteRequest)
      } yield {
        petRepo.get(createPet.id.get).unsafeRunSync() shouldEqual None
      }).unsafeRunSync()
    }
  }

  test("list pet") {
    val (auth, petRoutes, petRepo) = getTestResources()

    forAll { (pet: Pet, user: AdminUser) =>
      (for {
        createRequest1 <- POST(pet, uri"/pets")
          .flatMap(auth.embedToken(user.value, _))
        createResponse1 <- petRoutes.run(createRequest1)
        createPet1 <- createResponse1.as[Pet]
        createRequest2 <- POST(pet.copy(name = pet.name + 2, id = pet.id.map(_ + 10)), uri"/pets")
          .flatMap(auth.embedToken(user.value, _))
        createResponse2 <- petRoutes.run(createRequest2)
        createPet2 <- createResponse2.as[Pet]
        listRequest <- GET(uri"/pets")
          .flatMap(auth.embedToken(user.value, _))
        listResponse <- petRoutes.run(listRequest)
        listPet <- listResponse.as[List[Pet]]
      } yield {
        listPet.size shouldEqual 2
        petRepo.delete(createPet1.id.get)
        petRepo.delete(createPet2.id.get)
      }).unsafeRunSync()
    }
  }
}
