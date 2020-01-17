pipeline {
    agent {
        any
    }
    stages {
        stage('Build') {
            steps {
                sh './autogen && ./configure && make'
            }
        }
    }
}
