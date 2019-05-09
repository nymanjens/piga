package app.flux.react.app.document

import utest._

object MobileTaskEditorTest extends TestSuite {

  override def tests = TestSuite {
    val editor = (new Module).mobileTaskEditor

    "deriveReplacementString" - {
      editor.deriveReplacementString(oldContent = "abc", newContent = "def") ==> ""
      editor.deriveReplacementString(oldContent = "abc", newContent = "abcd") ==> "d"
      editor.deriveReplacementString(oldContent = "abc", newContent = "abc def") ==> " def"
    }
  }

  private class Module extends app.common.testing.TestModule {
    val mobileTaskEditor = new MobileTaskEditor
  }
}
