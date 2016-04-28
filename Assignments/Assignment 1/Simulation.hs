module Simulation (moveParticle, accelerate, advanceWorld) where
  
import World
import Physics

-- Move a particle according to its velocity for the given number of (simulated) seconds.
--
moveParticle :: Float -> Particle -> Particle
moveParticle dt (Particle m pos1@(x1, y1) vel@(vx, vy))
  = Particle m (x1 + delta_rx, y1 + delta_ry) vel
    where 
       delta_rx = vx * dt
       delta_ry = vy * dt

-- Accelerate a particle in dependence on the gravitational force excerted by all other particles for
-- the given number of (simulated) seconds.
--
accelerate :: Float -> [Particle] -> [Particle]
accelerate dt [] = []
accelerate dt [x] = [calculateVelocity dt x x]
accelerate dt particles@(particle:otherParticles) = particle : accelerate dt otherParticles
  where 
     particle = calculateNewVelocityForAllParticles dt particle particles

-- Helper function for the accelerate function.
-- This calculates the new velocity of a particle given:
-- the number of seconds that pass in the current timestep (delta_t)
-- two particles.
-- This essentially calculates the new velocity of the first particle on the basis of the mass and position of the second particle.
calculateVelocity :: Float -> Particle -> Particle -> Particle
calculateVelocity dt particleOne@(Particle m pos (vx, vy)) particleTwo = newParticle
   where 
     (ax, ay) = force particleOne particleTwo
     delta_vx = ax * dt
     delta_vy = ay * dt
     newParticle = Particle m pos (vx + delta_vx, vy + delta_vy)

-- Helper function for the accelerate function.
-- This calculates the new velocity for a particle given:
-- the number of seconds that pass in the current timestep (delta_t)
-- a particle
-- a list of particles
-- This essentially computes the effect of all the other particles on given particle's velocity.     
calculateNewVelocityForAllParticles :: Float -> Particle -> [Particle] -> Particle
calculateNewVelocityForAllParticles dt particle [] = particle
calculateNewVelocityForAllParticles dt particle [x] = calculateVelocity dt particle x
calculateNewVelocityForAllParticles dt particle (nextParticle:otherParticles) = calculateNewVelocityForAllParticles dt newParticle otherParticles
    where 
        newParticle = calculateVelocity dt particle nextParticle

-- Progressing the world state
--
advanceWorld :: unused -> Float -> World -> World
advanceWorld _ dt world@(World s1 s2 s3 []) = world
advanceWorld _ dt world@(World s1 s2 s3 [x]) = world
advanceWorld _ dt world@(World s1 s2 s3 (x:xs)) = newWorld
   where
     scaledTime = s3 * dt
     (y:ys)     = map (moveParticle scaledTime) (accelerate scaledTime (x:xs))
     newWorld   = World s1 s2 s3 (y:ys)



