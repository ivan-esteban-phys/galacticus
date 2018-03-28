!! Copyright 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018
!!    Andrew Benson <abenson@carnegiescience.edu>
!!
!! This file is part of Galacticus.
!!
!!    Galacticus is free software: you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.
!!
!!    Galacticus is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.
!!
!!    You should have received a copy of the GNU General Public License
!!    along with Galacticus.  If not, see <http://www.gnu.org/licenses/>.

!% Contains a module which implements algorithms for the proposal size in differential evolution algorithms.

module Posterior_Sample_Differential_Proposal_Size
  !% Implements algorithms for the proposal size in differential evolution algorithms.
  use Posterior_Sampling_State
  use Posterior_Sampling_Convergence
  private

  !# <functionClass>
  !#  <name>posteriorSampleDffrntlEvltnProposalSize</name>
  !#  <descriptiveName>Posterior Sampling Differential Evolution Proposal Size</descriptiveName>
  !#  <description>Class providing proposal sizes for differential evolution posterior samplers.</description>
  !#  <method name="gamma" >
  !#   <description>Sample from the jump distribution.</description>
  !#   <type>double precision</type>
  !#   <pass>yes</pass>
  !#   <argument>class(posteriorSampleStateClass      ), intent(inout) :: simulationState</argument>
  !#   <argument>class(posteriorSampleConvergenceClass), intent(inout) :: simulationConvergence</argument>
  !#  </method>
  !# </functionClass>

end module Posterior_Sample_Differential_Proposal_Size