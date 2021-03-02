package app.common

import scala.collection.JavaConverters._
import com.google.common.truth.Truth.assertThat
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import com.google.testing.junit.testparameterinjector.TestParameterInjector
import com.google.testing.junit.testparameterinjector.TestParameter
import com.google.testing.junit.testparameterinjector.TestParameters

@RunWith(classOf[TestParameterInjector])
class CaseFormatsTest {

  @Test
  @TestParameters(
    Array(
      "{input: ab_c, expected: [ab, c]}",
      "{input: abC, expected: [ab, c]}",
    )
  )
  def tokenize_success(input: String, expected: java.util.List[String]) = {
    assertThat(CaseFormats.tokenize(input).asJava) containsExactlyElementsIn expected
  }

  @Test
  def testMethod(@TestParameter(Array("A", "B")) variable: String) = {
    assertThat(variable) isEqualTo "B"
  }
}
