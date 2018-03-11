package com.joragupra.validators

import com.joragupra.validators.UrlTestUtils._
import com.netaporter.uri.Uri.parse
import com.netaporter.uri.config.UriConfig
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{MustMatchers, PropSpec, WordSpec}

import scalaz.{-\/, \/-}

class UrlValidatorSpec extends WordSpec with MustMatchers {

  val validator = new UrlValidator

  "validate" should {

    "return UrlValidationError" when {
      "protocol is missing" in {
        val urlWithoutProtocol = "www.google.com"

        val result = validator.validate(urlWithoutProtocol)

        result mustBe -\/(UrlValidationError("Url does not have a supported protocol"))
      }

      "url is longer than 300 characters" in {
        val tooLongUrl = "a" * 301

        val result = validator.validate(tooLongUrl)

        result mustBe -\/(UrlValidationError("Url too long"))
      }

      "url contains user" in {
        val urlWithUsernameAndPassword = "https://user@my.domain.com?asdf=qwer"

        val result = validator.validate(urlWithUsernameAndPassword)

        result mustBe -\/(UrlValidationError("Url can't contain user and password"))
      }

      "url contains user and password" in {
        val urlWithUsernameAndPassword = "https://user:password@my.domain.com?asdf=qwer"

        val result = validator.validate(urlWithUsernameAndPassword)

        result mustBe -\/(UrlValidationError("Url can't contain user and password"))
      }

      "the url can NOT be parsed" in {
        val invalidUrl = "https://:password@my.domain.com?asdf=qwer"

        val validationResult = validator.validate(invalidUrl)

        validationResult mustBe -\/(UrlValidationError("Url does not have a valid structure"))
      }

      "the url points to localhost" in {
        val invalidUrl = "https://localhost"

        val validationResult = validator.validate(invalidUrl)

        validationResult mustBe -\/(UrlValidationError("Url should not point to localhost"))
      }

      "the host of the URL is empty (IPv6)" in {
        val ipv6Url = "2001:0db8:0000:0042:0000:8a2e:0370:7334"

        val validationResult = validator.validate(ipv6Url)

        validationResult mustBe -\/(UrlValidationError("Url does not have a supported protocol"))
      }

      "the host of the URL contains only numbers (IPv4)" in {
        val ipv4Url = "http://223.255.255.254"

        val validationResult = validator.validate(ipv4Url)

        validationResult mustBe -\/(UrlValidationError("Url can not contain IPv4 addresses"))
      }
    }

    "return UrlValidationSuccess" when {
      "protocol is http" in {
        val httpUrl = "http://www.google.com"

        val result = validator.validate(httpUrl)

        result mustBe \/-(UrlValidationSuccess(httpUrl))
      }

      "protocol is https" in {
        val httpUrl = "HTTPS://www.google.com"

        val result = validator.validate(httpUrl)

        result mustBe \/-(UrlValidationSuccess(httpUrl))
      }

      "the url has parameters" in {
        val url = "HTTPs://उदाहरण.älteren.com?key=value?+<>\" \'"
        val encodedUrl = "HTTPs://उदाहरण.älteren.com?key=value%3F%2B%3C%3E%22%20%27"

        val validationResult = validator.validate(url)

        validationResult mustBe \/-(UrlValidationSuccess(encodedUrl))
      }

      "the url contains localhost but is another domain" in {
        val url = "https://xalocalhost"

        val validationResult = validator.validate(url)

        validationResult mustBe \/-(UrlValidationSuccess(url))
      }
    }
  }

}

class UrlValidationsSpec extends PropSpec with GeneratorDrivenPropertyChecks with MustMatchers {

  private implicit val config = UriConfig.conservative

  property("validateLength accepts URLs with 300 chars or less") {
    forAll(stringsWithLengthFromToGenerator(1, 299)) {
      (s: String) => validator.validateLength(parse(s)) must be(true)
    }
  }

  property("validateLength rejects URLs with more than 300 chars") {
    forAll(stringsWithLengthFromToGenerator(300, 1000)) {
      (s: String) => validator.validateLength(parse(s)) must be(false)
    }
  }

  property("validateProtocol accepts URLs with HTTP protocol") {
    forAll(stringsWithLengthFromToGenerator(1, 200)) {
      (s: String) => validator.validateProtocol(parse(withHttpsProtocol(s))) must be(true)
    }
  }

  property("validateProtocol accepts URLs with HTTPS protocol") {
    forAll(stringsWithLengthFromToGenerator(1, 200)) {
      (s: String) => validator.validateProtocol(parse(withHttpsProtocol(s))) must be(true)
    }
  }

  property("validateProtocol accepts URLs without HTTP and HTTPS protocol") {
    forAll(stringsWithLengthFromToGeneratorNotContainingGenerator(1, 200, Seq("http://", "https://"))) {
      (s: String) => validator.validateProtocol(parse(s)) must be(false)
    }
  }

  property("validateIsNotLocalhost accepts URLs with containing 'localhost' if it is not the only word in host name") {
    forAll(stringsWithLengthFromToGenerator(1, 5), stringsWithLengthFromToGenerator(1, 5)) {
      (prefix: String, suffix: String) => validator.validateIsNotLocalhost(parse(withHttpProtocol(prefix + "localhost" + suffix))) must be(true)
    }
  }

  property("validateIsNotLocalhost accepts URLs not containing 'localhost'") {
    forAll(stringsWithLengthFromToGeneratorNotContainingGenerator(1, 200, Seq("localhost"))) {
      (s: String) => validator.validateIsNotLocalhost(parse(withHttpProtocol(s))) must be(true)
    }
  }

  property("validateIsNotLocalhost rejects URLs if host name is only 'localhost'") {
    validator.validateIsNotLocalhost(parse(withHttpProtocol("localhost"))) must be(false)
  }

  property("validateNoUserAndPassword accepts URLs not containing username and password") {
    forAll(stringsWithLengthFromToGeneratorNotContainingGenerator(1, 200, Seq(":", "@"))) {
      (s: String) => validator.validateIsNotLocalhost(parse(withHttpProtocol(s))) must be(true)
    }
  }

  property("validateNoUserAndPassword rejects URLs with containing username and password information") {
    forAll(stringsWithLengthFromToGenerator(1, 5), stringsWithLengthFromToGenerator(1, 5), stringsWithLengthFromToGenerator(1, 200)) {
      (username: String, password: String, host: String) => validator.validateNoUserAndPassword(parse(withHttpProtocol(username + ":" + password + "@" + host))) must be(false)
    }
  }

}
