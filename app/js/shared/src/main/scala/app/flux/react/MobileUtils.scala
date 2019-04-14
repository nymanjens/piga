package app.flux.react

import hydro.common.LoggingUtils.logExceptions
import org.scalajs.dom

import scala.scalajs.js
import scala.util.matching.Regex

object MobileUtils {

  private val androidRegex: Regex = "android".r
  private val blackBerryRegex: Regex = "blackberry".r
  private val iOSRegex: Regex = "iphone|ipad|ipod".r
  private val operaRegex: Regex = "opera mini".r
  private val windowsRegex1: Regex = "iemobile".r
  private val windowsRegex2: Regex = "wpdesktop".r

  lazy val isMobileOrTablet: Boolean = logExceptions {
    val navigator = dom.window.navigator
    val userAgent = maybeAsString(navigator.userAgent)
    val vendor = maybeAsString(navigator.asInstanceOf[js.Dynamic].vendor)
    val opera = maybeAsString(dom.window.asInstanceOf[js.Dynamic].opera)

    val stringToTest = userAgent orElse vendor orElse opera getOrElse ""

    stringToTest.toLowerCase match {
      case androidRegex()    => true
      case blackBerryRegex() => true
      case iOSRegex()        => true
      case operaRegex()      => true
      case windowsRegex1()   => true
      case windowsRegex2()   => true
      case _                 => false
    }
  }

  private def maybeAsString(value: js.Any): Option[String] = {
    if (js.isUndefined(value)) {
      None
    } else {
      Some(value.asInstanceOf[String])
    }
  }
}
