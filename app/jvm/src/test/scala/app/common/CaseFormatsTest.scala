package app.common

import com.google.common.truth.Truth.assertThat
import com.google.testing.junit.testparameterinjector.TestParameter
import com.google.testing.junit.testparameterinjector.TestParameterInjector
import com.google.testing.junit.testparameterinjector.TestParameters
import org.junit.runner.RunWith
import org.junit.Test

import scala.collection.JavaConverters._

@RunWith(classOf[TestParameterInjector])
class CaseFormatsTest {

  @Test
  @TestParameters(
    Array(
      // snake_case
      "{input: ab_c, expected: [ab, c]}",
      // CamelCase
      "{input: abC, expected: [ab, c]}",
      "{input: AbCdE, expected: [ab, cd, e]}",
      "{input: AbCdEF, expected: [ab, cd, e, f]}",
      // dash-case
      "{input: ab-C, expected: [ab, c]}",
      // Other
      "{input: THE angryBird, expected: [the, angry, bird]}",
      "{input: 'a;b,c:-=+', expected: [a, b, c]}",
      "{input: a1, expected: [a1]}",
    )
  )
  def tokenize_success(input: String, expected: java.util.List[String]) = {
    assertThat(CaseFormats.tokenize(input).asJava) containsExactlyElementsIn expected
  }

  @Test
  @TestParameters(
    Array(
      "{input: [a], expected: A}",
      "{input: [ab], expected: Ab}",
      "{input: [ab, c], expected: AbC}",
      "{input: [ab, c, d], expected: AbCD}",
    )
  )
  def toUpperCamelCase_success(input: java.util.List[String], expected: String) = {
    assertThat(CaseFormats.toUpperCamelCase(input.asScala)) isEqualTo expected
  }

  @Test
  @TestParameters(
    Array(
      "{input: [a], expected: a}",
      "{input: [ab], expected: ab}",
      "{input: [ab, c], expected: ab_c}",
      "{input: [ab, c, d], expected: ab_c_d}",
    )
  )
  def toSnakeCase_success(input: java.util.List[String], expected: String) = {
    assertThat(CaseFormats.toSnakeCase(input.asScala)) isEqualTo expected
  }


  @Test
  @TestParameters(
    Array(
      "{input: [a], expected: a}",
      "{input: [ab], expected: ab}",
      "{input: [ab, c], expected: ab-c}",
      "{input: [ab, c, d], expected: ab-c-d}",
    )
  )
  def toDashCase_success(input: java.util.List[String], expected: String) = {
    assertThat(CaseFormats.toDashCase(input.asScala)) isEqualTo expected
  }
}
