package com.joragupra.validators

import com.netaporter.uri.Uri
import com.netaporter.uri.Uri.parse
import com.netaporter.uri.config.UriConfig
import scalaz.{-\/, \/, \/-}

case class UrlValidationSuccess(validatedUrl: String)
case class UrlValidationError(message: String)

class UrlValidator {

  private implicit val config = UriConfig.conservative

  def validate(urlString: String): \/[UrlValidationError, UrlValidationSuccess] = {
    try {
      val uri = parse(urlString)

      for {
        _ <- liftToEither(validateLength, "Url too long")(uri)
        _ <- liftToEither(validateProtocol, "Url does not have a supported protocol")(uri)
        _ <- liftToEither(validateIsNotLocalhost, "Url should not point to localhost")(uri)
        _ <- liftToEither(validateNoUserAndPassword, "Url can't contain user and password")(uri)
        r <- liftToEither(validateIsNotAnIPv4Address, "Url can not contain IPv4 addresses")(uri)
      } yield {
        r
      }
    } catch {
      case _: java.net.URISyntaxException => -\/(UrlValidationError("Url does not have a valid structure"))
    }
  }

  private def liftToEither(validation: Uri => Boolean, errorMsg: String): Uri => \/[UrlValidationError, UrlValidationSuccess] = {
    uri: Uri => {
      if (validation(uri)) \/-(UrlValidationSuccess(uri.toString))
      else -\/(UrlValidationError(errorMsg))
    }
  }

  def validateLength(uri: Uri): Boolean = {
    val urlString = uri.toStringRaw
    if (urlString.length() <= 300) {
      true
    }
    else {
      false
    }
  }

  def validateProtocol(uri: Uri): Boolean = {
    uri.protocol match {
      case Some(protocol) if protocol.equalsIgnoreCase("http") || protocol.equalsIgnoreCase("https") => true
      case _ => false
    }
  }

  def validateIsNotAnIPv4Address(uri: Uri): Boolean = {
    uri.host match {
      case Some(host) if host.matches("[0-9.]+") => false
      case _ => true
    }
  }

  def validateIsNotLocalhost(uri: Uri): Boolean = {
    val urlToLocalhost = uri.host.contains("localhost")
    if (!urlToLocalhost) {
      true
    } else {
      false
    }
  }

  def validateNoUserAndPassword(uri: Uri): Boolean = {
    (uri.user, uri.password) match {
      case (Some(_), None) => false
      case (Some(_), Some(_)) => false
      case _ => true
    }
  }

}

