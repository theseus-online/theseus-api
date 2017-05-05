module Kubernetes.Settings
    ( deployments
    , deploymentOf
    , deploymentsOf
    , replicasetsOf
    , pods
    , podsOf
    , logsOf
    , services
    , serviceOf
    , servicesOf
    , ingresses
    , ingressOf
    , ingressesOf
    , namespaces
    , networkPolicyOf
    , networkPoliciesOf
    , volumeRoot
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

pods :: String
pods = apiV1 ++ "/pods"

podsOf :: String -> String
podsOf namespace = apiV1 ++ "/namespaces/" ++ namespace ++ "/pods"

logsOf :: String -> String -> String
logsOf namespace pod = apiV1 ++ "/namespaces/" ++ namespace ++ "/pods/" ++ pod ++ "/log"

services :: String
services = apiV1 ++ "/services"

servicesOf :: String -> String
servicesOf namespace = apiV1 ++ "/namespaces/" ++ namespace ++ "/services"

serviceOf :: String -> String -> String
serviceOf namespace name = servicesOf namespace ++ "/" ++ name

ingresses :: String
ingresses = apiExtensions ++ "/ingresses"

ingressesOf :: String -> String
ingressesOf namespace = apiExtensions ++ "/namespaces/" ++ namespace ++ "/ingresses"

ingressOf :: String -> String -> String
ingressOf namespace name = ingressesOf namespace ++ "/" ++ name

namespaces :: String
namespaces = apiV1 ++ "/namespaces"

networkPoliciesOf :: String -> String
networkPoliciesOf namespace = apiExtensions ++ "/namespaces/" ++ namespace ++ "/networkpolicies"

networkPolicyOf :: String -> String -> String
networkPolicyOf namespace name = networkPoliciesOf namespace ++ "/" ++ name

volumeRoot :: FilePath
volumeRoot = "/theseus-volume"
