pipeline {
    agent any
    stages {
        stage('Build') {
            steps {
                sh './autogen && ./configure && make'
            }
        }
        stage('Tests') {
            steps {
                sh 'yes | ./spatch.opt --ctestall'
            }
        }
    }
    post {
        failure {
            emailext (
                subject: "Coccinelle CI failure: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]'",
                body: """
                    Failure: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]'.
                    Check console output at ${env.BUILD_URL}
                    """,
                recipientProviders: [[$class: 'DevelopersRecipientProvider']]
            )
        }
    }
}
