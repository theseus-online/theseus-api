module Kubernetes.Settings
    ( deployments
    , deploymentOf
    , deploymentsOf
    , replicasetsOf
    , pods
    , podsOf
    , services
    , serviceOf
    , servicesOf
    , ingresses
    , ingressOf
    , ingressesOf
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

volumeRoot :: FilePath
volumeRoot = "/theseus-volume"
