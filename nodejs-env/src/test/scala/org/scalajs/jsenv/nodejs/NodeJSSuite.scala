package org.scalajs.jsenv.nodejs

import org.scalajs.jsenv.test._

import org.junit.runner.RunWith

@RunWith(classOf[JSEnvSuiteRunner])
class NodeJSSuite extends JSEnvSuite(
    JSEnvSuiteConfig(new NodeJSEnv)
      .withTerminateVMJSCode("process.exit(0)")
)
