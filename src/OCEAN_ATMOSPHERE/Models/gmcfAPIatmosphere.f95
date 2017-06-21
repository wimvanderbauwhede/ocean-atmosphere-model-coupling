#define GMCF_VERBOSE
#define GMCF_INTERPOL_SPACE
module gmcfAPIatmosphere
    use gmcfAPI
    use gmcfInterpolation

    implicit none

    integer :: atmosphere_id
    integer :: sync_done, has_packets, fifo_empty
    integer :: t_sync, sim_time, current_sync_config, n_ticks_at_sync
    logical :: is_sync_point

    ! All time-related values are multiples of GMCF_TIME_QUANTUM
    integer, parameter :: GMCF_TIME_STEP_ATMOSPHERE = 24 ! number of quanta
    integer, parameter :: GMCF_TIME_OFFSET_ATMOSPHERE = 0 ! number of quanta
    ! I want the atmosphere domain to be twice as large as the ocean domain but with 1/5 of the resolution
    ! So if the ocean is 100 x 100, the atmosphere will be 200/5 x 200/5
    ! Now suppose I want the ocean to cover from 30/5 to 130/5 and from 40/5 to 140/5

!    integer, parameter :: ATMOSPHERE_NX=48,ATMOSPHERE_NY=48,ATMOSPHERE_KP=27

    integer, parameter :: ATMOSPHERE_NX=148,ATMOSPHERE_NY=148,ATMOSPHERE_KP=27

! Set up a subdomain of the atmosphere domain for transfer to the ocean model
! call gmcfExtract2DSubdomain(corner_indices, domain, subdomain)

    integer, parameter ::  ATMOSPHERE_SUB_NX=5 !100
    integer, parameter ::  ATMOSPHERE_SUB_NY=5! 100

    integer, parameter ::  ATMOSPHERE_SUB_IB=25
    integer, parameter ::  ATMOSPHERE_SUB_IE=29 !124
    integer, parameter ::  ATMOSPHERE_SUB_JB=25
    integer, parameter ::  ATMOSPHERE_SUB_JE=29 !124

    integer, dimension(4) :: gmcfAtmosphereSubdomainCorners = (/ ATMOSPHERE_SUB_IB, ATMOSPHERE_SUB_IE, ATMOSPHERE_SUB_JB, ATMOSPHERE_SUB_JE /)

!    integer, parameter :: ATMOSPHERE_DX=5,ATMOSPHERE_DY=5,ATMOSPHERE_DK=1  ! note that the latter is usually non-linear!
    ! I'm not happy with these names. The correct approach would be to define OCEAN_OFFSET_FROM_ATMOSPHERE_X, OCEAN_OFFSET_FROM_ATMOSPHERE_Y
!    integer, parameter :: ATMOSPHERE_X0=6,ATMOSPHERE_Y0=8,ATMOSPHERE_KMS=1
!    integer, parameter :: ATMOSPHERE_IME=26,ATMOSPHERE_JME=28,ATMOSPHERE_KME=27

    type(gmcfPacket) :: packet

    ! These arrays are used for transfer between the models only
    real(4), dimension(1:4,0:ATMOSPHERE_SUB_NX-1,0:ATMOSPHERE_SUB_NY-1) :: wind_profile
    real(4), dimension(0:ATMOSPHERE_SUB_NX-1,0:ATMOSPHERE_SUB_NY-1) :: temperature
    save

contains
! ----------------------------------------------------------------------------------------------------------------
! It would seem that we don't need this call anymore because all we need is
! call gmcfInitCoupler(sys,tile, atmosphere_id)
    subroutine gmcfInitAtmosphere(sys,tile,m_id)
        integer(8), intent(In) :: sys
        integer(8), intent(In) :: tile
        integer, intent(In) :: m_id
#ifdef GMCF_VERBOSE
        print *, "ATMOSPHERE API gmcfInitAtmosphere BEFORE gmcfInitCoupler()"
#endif

        atmosphere_id=m_id
!        t_atmosphere = 0
!        t_sync_prev = -1
!        t_sync = t_atmosphere
!        t_sync_step = 24
        call gmcfInitCoupler(sys,tile, atmosphere_id)
    end subroutine gmcfInitAtmosphere
! ----------------------------------------------------------------------------------------------------------------
! n_ticks = (t-t_start)/t_step
    subroutine gmcfSyncAtmosphere(n_ticks) ! (var_name_1, var_name_2)
    ! t_atmosphere is the number of quanta elapsed since the start of the simulation
    ! in other words
        integer, intent(In) :: n_ticks

!        t_sync = n_ticks / t_sync_step ! means atmosphere steps are smaller and more frequent than ocean
!        t_inter = mod(n_ticks,t_sync_step) ! for interpolation between two steps of atmosphere
!        n_ticks = n_ticks + 1

        sim_time = n_ticks * GMCF_TIME_STEP_ATMOSPHERE + GMCF_TIME_OFFSET_ATMOSPHERE
#ifdef GMCF_VERBOSE
        print *, "ATMOSPHERE API gmcfSyncAtmosphere",sim_time
#endif

        call gmcfCheckSync(atmosphere_id,sim_time,current_sync_config, is_sync_point)

        if (is_sync_point) then
        ! Arguably as soon as a model has finished, there is no point in interacting with it
        ! So this should actually extend to all models.
        ! A nice solution might be a per-model bitmask, so that we check simply if it is zero
            if (gmcfStatus(GMCF_OCEAN_ID) /= FIN) then
#ifdef GMCF_VERBOSE
                print *, "ATMOSPHERE API BEFORE gmcfSync()",sim_time,sync_done
#endif
                sync_done=0
                do while(sync_done == 0)
                    call gmcfSync(atmosphere_id,sim_time,sync_done)
#ifdef GMCF_VERBOSE
                    print *, "ATMOSPHERE API AFTER gmcfSync()"
#endif

                    if (sync_done == 0) then
#ifdef GMCF_VERBOSE
                        print *, "ATMOSPHERE API SYNC NOT DONE!"
#endif

!                select case (gmcfDataRequests(atmosphere_id)%data_id) ! <code for the variable var_name, GMCF-style>
!                    case (GMCF_WIND_PROFILE)
!                        call gmcfSend3DFloatArray(atmosphere_id,wind_profile, shape(wind_profile), GMCF_WIND_PROFILE, gmcfDataRequests(atmosphere_id)%source,PRE,t_sync)
!                end select

                    end if
#ifdef GMCF_VERBOSE
                    print *, "ATMOSPHERE API", atmosphere_id," sync loop ",sim_time,"..."
#endif

                end do
#ifdef GMCF_VERBOSE
                print *, "ATMOSPHERE API", atmosphere_id," syncing DONE for time step ",sim_time, ", ", n_ticks, " ticks"
#endif
            end if ! FIN
        end if ! is_sync_point

    end subroutine gmcfSyncAtmosphere
! ----------------------------------------------------------------------------------------------------------------
    subroutine gmcfPreAtmosphere(u,v,w, t_surface)
        real(kind=4), dimension(ATMOSPHERE_NX,ATMOSPHERE_NY), intent(InOut) :: t_surface
        real(kind=4), dimension(ATMOSPHERE_NX,ATMOSPHERE_NY,ATMOSPHERE_KP), intent(In) :: u,v,w
        if (is_sync_point) then
        ! Now, it could be that FIN is reached here for DEST_1, so we should stop here
        if (gmcfStatus(GMCF_OCEAN_ID) /= FIN) then
                select case (current_sync_config) ! <code for the variable var_name, GMCF-style>
                    case (ATMOSPHERE_OCEAN_WIND_PROFILE)
                        ! so, of course I should assign the wind profile here
                        ! The wind_profile is a subdomain of u,v,w
                        call gmcfCreateWindprofileAtmosphere(u,v,w)
                        print *, "ATMOSPHERE API: gmcfCreateWindprofileAtmosphere(u,v,w) BB",u(gmcfAtmosphereSubdomainCorners(1),gmcfAtmosphereSubdomainCorners(3),1),wind_profile(1,0,0)
                        print *, "ATMOSPHERE API: gmcfCreateWindprofileAtmosphere(u,v,w) EE",u(gmcfAtmosphereSubdomainCorners(2),gmcfAtmosphereSubdomainCorners(4),1),wind_profile(1,ATMOSPHERE_SUB_NX-1,ATMOSPHERE_SUB_NY-1)
                        print *, "ATMOSPHERE API: gmcfSend3DFloatArray(wind_profile)"
                        call gmcfSend3DFloatArray(atmosphere_id,wind_profile, shape(wind_profile), GMCF_WIND_PROFILE, GMCF_OCEAN_ID,PRE,sim_time)
!                    case (OCEAN_ATMOSPHERE_TEMPERATURE)
                        call gmcfWaitFor(atmosphere_id,RESPDATA, GMCF_OCEAN_ID, 1)
#ifdef GMCF_VERBOSE
                        print *, "ATMOSPHERE API: got 1 DRESP ..."
#endif
                        ! and then we read them
                        call gmcfHasPackets(atmosphere_id,RESPDATA,has_packets)
                        do while (has_packets==1)
                            call gmcfShiftPending(atmosphere_id,GMCF_OCEAN_ID,RESPDATA,packet,fifo_empty)
                            ! read a packet
                            select case (packet%data_id) ! <code for the variable var_name, GMCF-style>
                                case (GMCF_TEMPERATURE)
                                    call gmcfRead2DFloatArray(temperature,shape(temperature), packet)
                                    print *, "ATMOSPHERE API: gmcfRead2DFloatArray(temperature)"
                                    call gmcfReceiveTemperatureAtmosphere(t_surface)
                                    print *, "ATMOSPHERE API: gmcfReceiveTemperatureAtmosphere(t_surface) BB",t_surface(gmcfAtmosphereSubdomainCorners(1),gmcfAtmosphereSubdomainCorners(3)),temperature(0,0)
                                    print *, "ATMOSPHERE API: gmcfReceiveTemperatureAtmosphere(t_surface) EE",t_surface(gmcfAtmosphereSubdomainCorners(2),gmcfAtmosphereSubdomainCorners(4)),temperature(ATMOSPHERE_SUB_NX-1,ATMOSPHERE_SUB_NY-1), sum(temperature), sum(t_surface)
                            end select
                            call gmcfHasPackets(atmosphere_id,RESPDATA,has_packets)
                        end do
                        print *, "ATMOSPHERE API: SANITY: ",sum(temperature)
#ifdef GMCF_VERBOSE
                        print *, "ATMOSPHERE API: DONE reading DRESP into vars, ready to compute ..."
#endif
                    case default
                        print *, "ATMOSPHERE API: current_sync_config = ",current_sync_config," => NO ACTION"
                    ! probably need a default "do nothing" here
                end select
        end if ! FIN
        end if ! is_sync_point
    end subroutine gmcfPreAtmosphere

! ----------------------------------------------------------------------------------------------------------------
    subroutine gmcfPostAtmosphere

        if (gmcfStatus(GMCF_OCEAN_ID) /= FIN) then
        ! Wait for one post data request
#ifdef GMCF_VERBOSE
        print *,"ATMOSPHERE API: POST SHOULD BE  OBSOLETE!"
#endif
        end if ! FIN
    end subroutine gmcfPostAtmosphere
! ----------------------------------------------------------------------------------------------------------------
    ! This routine takes the actual u,v,w as inputs and populates wind_profile, which is a 2-D array of 4-tuples
    ! Problem is that we also need to know the start inside the mem array, I guess that is ids etc
    ! We need to do this the proper way via the physical coordinates I think
    ! For shared memory it does not matter much but for message passing I should send just the points needed to do the interpolation
    ! This is actually a bit more tricky than it seems:
    ! - we need to compare the actual physical domains,
    ! - then find the points that are just outside this domain
    ! - then create an array slice using these points
    ! - So, create the arrays with coords for each point
    ! - go though the larger array until a coord is larger than the first coord in the smaller array. The point before is the one we need. Similar, the first point greater than the last of the smaller array is what we need.
    subroutine gmcfCreateWindprofileAtmosphere(u,v,w)
    ! The dimension of u,v,w is unsatisfactory, we need to get these from the original sources of course
        real(4), dimension(ATMOSPHERE_NX,ATMOSPHERE_NY,ATMOSPHERE_KP), intent(In) :: u,v,w
        ! TEST1 we take 1 point
!        wind_profile(1,0,0)=u(1,1,1)
!        wind_profile(2,0,0)=v(1,1,1)
!        wind_profile(3,0,0)=w(1,1,1)
!        wind_profile(4,0,0)=0.0
        ! TEST 2 we take the subdomain of u,v,w from the atmoshere as the wind_profile
        call gmcfExtract2DSubdomain(gmcfAtmosphereSubdomainCorners, u(:,:,1), wind_profile(1,:,:))
        call gmcfExtract2DSubdomain(gmcfAtmosphereSubdomainCorners, v(:,:,1), wind_profile(2,:,:))
        call gmcfExtract2DSubdomain(gmcfAtmosphereSubdomainCorners, w(:,:,1), wind_profile(3,:,:))
        ! TEST 3 we take u,v,w from a portion of the domain of the atmoshere as the wind_profile

!        integer :: i,j
!        do j=ATMOSPHERE_Y0,ATMOSPHERE_JME
!            do i=ATMOSPHERE_X0,ATMOSPHERE_IME
!                wind_profile(1,i-ATMOSPHERE_X0,j-ATMOSPHERE_Y0)=4.21 ! u(1,j,k)
!                wind_profile(2,i-ATMOSPHERE_X0,j-ATMOSPHERE_Y0)=4.22 ! v(1,j,k)
!                wind_profile(3,i-ATMOSPHERE_X0,j-ATMOSPHERE_Y0)=4.23 ! w(1,j,k)
!                wind_profile(4,i-ATMOSPHERE_X0,j-ATMOSPHERE_Y0)=0.0 ! for cache alignment
!            end do
!        end do
    end subroutine gmcfCreateWindprofileAtmosphere
! ----------------------------------------------------------------------------------------------------------------
    ! The temperature array received from the ocean model is sampled from the original temperature array in the ocean model
    ! This should happen before sending
    subroutine gmcfReceiveTemperatureAtmosphere(t_surface)
        real(kind=4), dimension(ATMOSPHERE_NX,ATMOSPHERE_NY), Intent(InOut) :: t_surface
#ifdef GMCF_INTERPOL_SPACE
        integer :: i,j
#endif
        ! In general, sampling requires interpolation. In the current approach the outer points of the temperature array will not be modified
        ! TEST 1
!        print *, "Reading TEMPERATURE",t_surface(1,1)
!        t_surface(1,1)=temperature(0,0)
        ! TEST 2
        call gmcfInsert2DSubdomain(gmcfAtmosphereSubdomainCorners,t_surface, temperature)
#ifndef GMCF_INTERPOL_SPACE
        print *, "gmcfInsert2DSubdomain:",t_surface(gmcfAtmosphereSubdomainCorners(1),gmcfAtmosphereSubdomainCorners(3)),'=',temperature(0,0),';',t_surface(gmcfAtmosphereSubdomainCorners(2),gmcfAtmosphereSubdomainCorners(4)),'=',temperature(ATMOSPHERE_SUB_NX-1,ATMOSPHERE_SUB_NY-1)
#else
        do i=0,ATMOSPHERE_SUB_NX-1
            do j=0,ATMOSPHERE_SUB_NY-1
                print *, "gmcfReceiveTemperatureAtmosphere:", i,j,temperature(i,j)
            end do
        end do
#endif
    end subroutine gmcfReceiveTemperatureAtmosphere
! ----------------------------------------------------------------------------------------------------------------
    subroutine gmcfFinishedAtmosphere
        call gmcfFinished(atmosphere_id)
    end subroutine gmcfFinishedAtmosphere


end module gmcfAPIatmosphere
