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
}
