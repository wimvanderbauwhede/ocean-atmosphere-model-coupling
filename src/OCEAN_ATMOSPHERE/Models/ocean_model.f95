#define MODEL_API
! This is the ocean model
! Ocean model: Delta t = 480 min
! Ocean is more fine-grained than Atmosphere
! => In ocean I must interpolate the Atmosphere values
! What the ocean model receives from the atmosphere model is, I assume, either (u,v,w) at the surface, or p at the surface, not sure which

subroutine program_ocean_gmcf(sys, tile, model_id) ! This replaces 'program main'

    ! Lines marked with ! gmcf-coupler / ! end gmcf-coupler are additions for coupling

    ! gmcf-coupler
    use gmcfAPI

    use gmcfAPIocean

    integer(8) , intent(In) :: sys
    integer(8) , intent(In) :: tile
    integer , intent(In) :: model_id
    ! end gmcf-coupler
    integer :: t,t_start,t_stop,t_step, ii, jj, kk
    integer :: can_interpolate
    real(kind=4) :: v1sum,v2sum
    real(kind=4), dimension(128) :: var_name_1_prev,var_name_1
    real(kind=4), dimension(128,128,128) ::  var_name_2_prev,var_name_2

    ! Simulation start, stop, step for model 1
    t_start = 0
    t_stop = 200
    t_step = 1

    t_sync_prev = -1 ! always
    t_sync = t_start
    t_sync_step = 20 !


    ! This flag is used to activate the interpolation functionality
    can_interpolate = 0

    ! gmcf-coupler
    ! Init amongst other things gathers info about the time loops, maybe from a config file, need to work this out in detail:
    call gmcfInitModel1(sys,tile, model_id)
    ! end gmcf-coupler

    print *, "FORTRAN MODEL1", model_id,"main routine called with pointers",sys,tile

    ! Compute initial
    var_name_1=0.0
    var_name_2=0.0

    do t = t_start,t_stop,t_step
        print *, "FORTRAN MODEL1", model_id," syncing for time step ",t,"..."
        ! gmcf-coupler
        ! Sync all models by requesting all timesteps, block until all received, and sending your timestep to all who ask, until everyone has asked?
        ! Is this possible? I think it is OK:
        ! Start by sending N-1 requests; read from the FIFO,
        ! block if there is nothing there.
        ! You'll get requests and/or data. For every request, send data; keep going until you've sent data to all and received data from all.
        ! I don't think this will deadlock.

        call gmcfSyncModel1(t, var_name_1,var_name_2)
        call gmcfPreModel1(var_name_1,var_name_1_prev,var_name_2,var_name_2_prev)
        ! end gmcf-coupler

        ! WV: Another complication is that we might need to interpolate the received values.
        ! WV: This will always be the case if the consumer time step is smaller than the producer
        ! WV: And we know this from the configuration.
        ! WV: So a guard should be generated around the computation.

        if (can_interpolate == 0) then
            can_interpolate = 1
        else
            ! And now we can do work on this data
            ! WV: I would prefer to introduce a copy step with the interpolated values, but that is yet another copy ...
            ! WV: In fact, the current full-size copy in the gmcffloatarrayfromptrc_ function should be removed!
            ! WV: And then we could do
            v1sum=0.0
            v2sum=0.0
            do ii=1,128
                v1sum = v1sum + (var_name_1_prev(ii)*t_inter + var_name_1(ii)*(t_sync_step-t_inter) )/t_sync_step
                do jj=1,128
                    do kk=1,128
                        v2sum = v2sum + ( var_name_2_prev(ii,jj,kk)*t_inter +var_name_2(ii,jj,kk)*(t_sync_step-t_inter) )/t_sync_step
                    end do
                end do
            end do
            print *, "FORTRAN MODEL1", model_id,"WORK DONE:",v1sum,v2sum
        end if

    end do ! time loop model 1
    call gmcfFinished(model_id)
    print *, "FORTRAN MODEL1", model_id,"main routine finished after ",t_stop - t_start," time steps"

end subroutine program_ocean_gmcf

