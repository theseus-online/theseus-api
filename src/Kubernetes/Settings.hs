module Kubernetes.Settings
    ( deployments
    , deploymentOf
    , deploymentsOf
    , replicasetsOf
    , podsOf
    ) where

apiV1 :: String
apiV1 = "http://theseus:8080/api/v1"

apiExtensions :: String
apiExtensions = "http://theseus:8080/apis/extensions/v1beta1"

deployments :: String
deployments = apiExtensions ++ "/deployments"

deploymentsOf :: String -> String
deploymentsOf namespace = apiExtensions ++ "/namespaces/" ++ namespace ++ "/deployments"

deploymentOf :: String -> String -> String
deploymentOf namespace name = deploymentsOf namespace ++ "/" ++ name

replicasetsOf :: String -> String
replicasetsOf namespace = apiExtensions ++ "/namespaces/" ++ namespace ++ "/replicasets"

podsOf :: String -> String
podsOf namespace = apiV1 ++ "/namespaces/" ++ namespace ++ "/pods"