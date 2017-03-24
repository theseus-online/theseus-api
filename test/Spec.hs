{-# LANGUAGE LambdaCase #-}
import Kubernetes.Deployments

main :: IO ()
main = do
    getDeploymentsOf "theseus-online" >>= \case
        Right x -> print x
        Left y -> print y
