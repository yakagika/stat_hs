module Poisson where

import Permutation


type N           = Integer
type X           = Integer
type P           = Double
type Probability = Double

bernoulli :: N -> X -> P -> Probability
bernoulli n x p = (fromIntegral (combinate n x))
                * (p ** fromIntegral x)
                * ((1 - p) ** fromIntegral (n - x))

poisson :: N -> X -> P -> Probability
poisson n x p
    =  let lambda = (fromIntegral n) * p
    in (exp (- lambda))
    *  (lambda ** fromIntegral x)
    /  (fromIntegral (factorial x))