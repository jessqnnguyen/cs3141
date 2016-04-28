module Test where

import Test.QuickCheck
import Simulation
import World
import Physics
import TestSupport

prop_EnergyConservation world  = (oldEnergy / newEnergy) < realToFrac World.epsilon
     where 
        oldEnergy = worldEnergy world
        newEnergy = worldEnergy $ advanceWorld 0 0.001 world