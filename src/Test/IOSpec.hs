module Test.IOSpec
  (
-- * The specifications
    module Test.IOSpec.Fork
  , module Test.IOSpec.MVar
  , module Test.IOSpec.IORef
  , module Test.IOSpec.STM
  , module Test.IOSpec.Teletype
-- * The basic types
  , module Test.IOSpec.Types 
-- * The virtual machine
  , module Test.IOSpec.VirtualMachine
  ) where

import Test.IOSpec.Fork
import Test.IOSpec.MVar
import Test.IOSpec.IORef
import Test.IOSpec.STM
import Test.IOSpec.Teletype
import Test.IOSpec.Types (IOSpec, (:+:)(..), inject, (:<:))
import Test.IOSpec.VirtualMachine
