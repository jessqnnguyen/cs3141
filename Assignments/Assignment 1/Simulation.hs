module Simulation (moveParticle, accelerate, advanceWorld) where
  
import World
import Physics

-- Move a particle according to its velocity for the given number of (simulated) seconds.
--
moveParticle :: Float -> Particle -> Particle
moveParticle delta_t (Particle m pos1@(x1, y1) (vx, vy))
  = Particle m (x1 +  delta_rx, y1 + delta_ry) (vx, vy)
    where 
       delta_rx = vx * delta_t
       delta_ry = vy * delta_t
     
moveAllParticles :: Float -> [Particle] -> [Particle]
moveAllParticles delta_t [] = []
moveAllParticles delta_t [x] = [moveParticle delta_t x]
moveAllParticles delta_t (x:xs) = moveParticle delta_t x : moveAllParticles delta_t xs

-- Accelerate a particle in dependence on the gravitational force excerted by all other particles for
-- the given number of (simulated) seconds.
--
accelerate :: Float -> [Particle] -> [Particle]
accelerate delta_t [] = []
accelerate delta_t (x:xs) = (Particle m2 pos2 v2) : accelerate delta_t xs
  where 
     Particle m1 pos1 v1 = x
     Particle m2 pos2 v2 = calculateNewVelocityForAllParticles delta_t (Particle m1 pos1 v1) (x:xs)

-- Helper function for the accelerate function.
-- This calculates the new velocity of a particle given:
-- the number of seconds that pass in the current timestep (delta_t)
-- two particles.
-- This essentially calculates the new velocity of the first particle on the basis of the mass and position of the second particle.
calculateVelocity :: Float -> Particle -> Particle -> Particle
calculateVelocity delta_t (Particle m1 pos1 (vx, vy)) (Particle m2 pos2 v2) = Particle m1 pos1 (vx + delta_vx, vy + delta_vy)
   where 
     a = force (Particle m1 pos1 (vx, vy)) (Particle m2 pos2 v2)
     (ax, ay) = a
     delta_vx = ax * delta_t
     delta_vy = ay * delta_t

-- Helper function for the accelerate function.
-- This calculates the new velocity for a particle given:
-- the number of seconds that pass in the current timestep (delta_t)
-- a particle
-- a list of particles
-- This essentially computes the effect of all the other particles on given particle's velocity.     
calculateNewVelocityForAllParticles :: Float -> Particle -> [Particle] -> Particle
calculateNewVelocityForAllParticles delta_t (Particle m1 pos1 v1) [] = Particle m1 pos1 v1
calculateNewVelocityForAllParticles delta_t (Particle m1 pos1 v1) [x] = calculateVelocity delta_t (Particle m1 pos1 v1) nextParticle 
  where
     (Particle m2 pos2 v2) = x
     nextParticle = (Particle m2 pos2 v2)
calculateNewVelocityForAllParticles delta_t (Particle m1 pos1 v1) (x:xs) = calculateNewVelocityForAllParticles delta_t (Particle m1 pos1 v1) xs
    where 
        Particle m2 pos2 v2 = x
        updateCurrParticle = calculateVelocity delta_t (Particle m1 pos1 v1) (Particle m2 pos2 v2)
        Particle m1 pos1 v1 = updateCurrParticle 

--data World = World Float Float Float [Particle]
--           deriving (Show, Read)/
-- Progressing the world state
--
advanceWorld :: unused -> Float -> World -> World
advanceWorld _ delta_t (World s1 s2 s3 (x:xs)) = World s1 s2 s3 (y:ys)
   where
     scaledTime = s3 * delta_t 
     (z:zs) = accelerate scaledTime (x:xs)
     (y:ys) = moveAllParticles scaledTime (z:zs)



