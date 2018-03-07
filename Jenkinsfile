def HOME = "/localhome/jenkins"
def LOC_SBT_BASE = "$HOME/scala-js-sbt-homes"
def LOC_SBT_BOOT = "$LOC_SBT_BASE/sbt-boot"
def LOC_SBT_HOME = "$LOC_SBT_BASE/sbt-home"

def SBT_OPTS="-J-Xmx3G -J-XX:MaxPermSize=512M -Djline.terminal=jline.UnsupportedTerminal -Dsbt.boot.directory=$LOC_SBT_BOOT -Dsbt.ivy.home=$LOC_SBT_HOME -Divy.home=$LOC_SBT_HOME -Dsbt.global.base=$LOC_SBT_BASE"

withEnv(["NODE_PATH=$HOME/node_modules/"]) {
    stage('Test') {
        parallel scala_2_11_12: {
            node('linuxworker') {
                checkout scm
                sh "sbt $SBT_OPTS ++2.11.12 helloworld/run"
            }
        },
        scala_2_12_4: {
            node('linuxworker') {
                checkout scm
                sh "sbt $SBT_OPTS ++2.12.4 helloworld/run"
            }
        }
    }
}
