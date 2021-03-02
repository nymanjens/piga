package app.common

import com.google.common.truth.Truth.assertThat
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class CaseFormatsTest  {

  @Test
  def myTest() = {
    assertThat(CaseFormats.tokenize("ab_c")) isEqualTo Seq("ab", "c")
  }
}
