#define GMCF_VERBOSE
module gmcfAPIocean
    use gmcfAPI
    use gmcfInterpolation

    implicit none

    integer :: ocean_id
    integer :: sync_done, has_packets, fifo_empty
    integer :: t_sync, t_sync_prev, t_sync_step, t_inter, n_ticks_at_sync
    integer :: sim_time,  current_sync_config
    logical :: is_sync_point

    ! All time-related values are multiples of GMCF_TIME_QUANTUM
    integer, parameter :: GMCF_TIME_STEP_OCEAN = 1 ! number of quanta
    integer, parameter :: GMCF_TIME_OFFSET_OCEAN = 0 ! number of quanta

    integer, parameter :: ATMOSPHERE_IP=148,ATMOSPHERE_JP=148,ATMOSPHERE_KP=27 ! Should this be here or in the model?
    integer, parameter :: ATMOSPHERE_DI=5,ATMOSPHERE_DJ=5,ATMOSPHERE_DK=1
#if GMCF_INTERPOL_SPACE
    ! I'm not happy with these names. The correct approach would be to define OCEAN_OFFSET_FROM_ATMOSPHERE_X, OCEAN_OFFSET_FROM_ATMOSPHERE_Y
    ! Or actually better, use physical coordinates
    integer, parameter :: ATMOSPHERE_IMS=6,ATMOSPHERE_JMS=8,ATMOSPHERE_KMS=1
    integer, parameter :: ATMOSPHERE_IME=26,ATMOSPHERE_JME=28,ATMOSPHERE_KME=27
#endif
!    integer, parameter :: OCEAN_IP=100,OCEAN_JP=100,OCEAN_KP=26
!    integer, parameter :: OCEAN_IP=48,OCEAN_JP=48,OCEAN_KP=27
    integer, parameter :: OCEAN_IP=100,OCEAN_JP=100,OCEAN_KP=27
#if GMCF_INTERPOL_SPACE
    integer, parameter :: OCEAN_DI=1,OCEAN_DJ=1,OCEAN_DK=1 ! note that the latter is usually non-linear!
    integer, parameter :: OCEAN_IMS=0,OCEAN_JMS=0,OCEAN_KMS=0
#endif

    integer, parameter ::  ATMOSPHERE_SUB_IP=100
    integer, parameter ::  ATMOSPHERE_SUB_JP=100


    type(gmcfPacket) :: packet
    real(4), dimension(1:4,0:ATMOSPHERE_SUB_IP-1,0:ATMOSPHERE_SUB_JP-1) :: wind_profile, wind_profile_prev, wind_profile_current
    real(4), dimension(0:ATMOSPHERE_SUB_IP-1,0:ATMOSPHERE_SUB_JP-1) :: temperature
    save

contains
! ----------------------------------------------------------------------------------------------------------------
!$GMCF Init(timesteps=20) !if >1 it means we need to interpolate otherwise not
    subroutine gmcfInitOcean(sys,tile,m_id)
        integer(8), intent(In) :: sys
        integer(8), intent(In) :: tile
        integer, intent(In) :: m_id
#ifdef GMCF_VERBOSE
        print *, "OCEAN API gmcfInitOcean() BEFORE gmcfInitCoupler"
#endif
        ocean_id=m_id
!        t_ocean = 0
!        t_sync_prev = -1
!        t_sync = t_ocean
!        t_sync_step = 24
        call gmcfInitCoupler(sys,tile, ocean_id)
    end subroutine gmcfInitOcean
! ----------------------------------------------------------------------------------------------------------------
! n_ticks = (t-t_start)/t_step
    subroutine gmcfSyncOcean(n_ticks)
        integer, intent(In) :: n_ticks
!        t_sync = t_ocean / t_sync_step ! means ocean steps are smaller and more frequent than atmosphere
!        t_inter = mod(t_ocean,t_sync_step) ! for interpolation between two steps of atmosphere
!        t_ocean = t_ocean + 1

        sim_time = n_ticks * GMCF_TIME_STEP_OCEAN + GMCF_TIME_OFFSET_OCEAN
#ifdef GMCF_VERBOSE
        print *, "OCEAN API gmcfSyncOcean",sim_time
#endif
        call gmcfCheckSync(ocean_id,sim_time,current_sync_config, is_sync_point)

        if (is_sync_point) then
            n_ticks_at_sync = n_ticks
            if (gmcfStatus(GMCF_ATMOSPHERE_ID) /= FIN) then
        ! Sync will synchronise simulation time steps but also handle any pending requests
        !$GMC sync(t)

#ifdef GMCF_VERBOSE
                print *, "OCEAN API BEFORE gmcfSync()",sim_time,sync_done
#endif
                    sync_done=0
                    do while(sync_done == 0)
                        call gmcfSync(ocean_id,sim_time,sync_done)
#ifdef GMCF_VERBOSE
                        print *, "OCEAN API AFTER gmcfSync()"
                        print *, "OCEAN API", ocean_id," sync loop ",sim_time,"..."
#endif
                    end do
#ifdef GMCF_VERBOSE
                print *, "OCEAN API", ocean_id," syncing DONE for time step ",sim_time, ", ", n_ticks, " ticks"
#endif
            end if ! FIN
        end if ! is_sync_point
    end subroutine gmcfSyncOcean
! ----------------------------------------------------------------------------------------------------------------
    subroutine gmcfPreOcean(u,v,w, t_surface)
        real(kind=4), dimension(OCEAN_IP,OCEAN_JP,OCEAN_KP), intent(InOut) :: u,v,w
        real(kind=4), dimension(OCEAN_IP,OCEAN_JP), intent(In) :: t_surface

        if (is_sync_point) then
            ! So now we can do some work. Let's suppose ocean is the OCEAN, and it requests data from model2, ATMOSPHERE.
            ! First overwrite the *prev vars with the current vars
#if GMCF_INTERPOL_TIME
        wind_profile_prev = wind_profile
#endif
        if (gmcfStatus(GMCF_OCEAN_ID) /= FIN) then
                select case (current_sync_config) ! <code for the variable var_name, GMCF-style>
                    case (OCEAN_ATMOSPHERE_TEMPERATURE)

                        call gmcfSampleTemperatureOcean(t_surface)
                        print *, "OCEAN API: gmcfSampleTemperatureOcean(t_surface) BB", t_surface(1,1), temperature(0,0)
                        print *, "OCEAN API: gmcfSampleTemperatureOcean(t_surface) EE", t_surface(OCEAN_IP,OCEAN_JP), temperature(ATMOSPHERE_SUB_IP-1,ATMOSPHERE_SUB_JP-1), sum(temperature), sum(t_surface)
                        print *, "OCEAN API: gmcfSend2DFloatArray(temperature)"
                        call gmcfSend2DFloatArray(ocean_id,temperature, shape(temperature), GMCF_TEMPERATURE, GMCF_ATMOSPHERE_ID,PRE,sim_time)
!                    case (ATMOSPHERE_OCEAN_WIND_PROFILE)
                        call gmcfWaitFor(ocean_id,RESPDATA, GMCF_ATMOSPHERE_ID, 1)
#ifdef GMCF_VERBOSE
                        print *, "OCEAN API: got 1 DRESP ..."
#endif
                        ! and then we read them
                        call gmcfHasPackets(ocean_id,RESPDATA,has_packets)
                        do while (has_packets==1)
                            call gmcfShiftPending(ocean_id,GMCF_ATMOSPHERE_ID,RESPDATA,packet,fifo_empty)
                            ! read a packet
                            select case (packet%data_id) ! <code for the variable var_name, GMCF-style>
                                case (GMCF_WIND_PROFILE)
                                    call gmcfRead3DFloatArray(wind_profile,shape(wind_profile), packet)
                                    print *, "OCEAN API: gmcfRead3DFloatArray(wind_profile)",wind_profile(1,0,0)
                                    ! so here we must assign the wind profile to u,v,w
                                    call gmcfInterpolateWindprofileSpaceOcean(u,v,w)
                                    print *, "OCEAN API: gmcfInterpolateWindprofileSpaceOcean(u,v,w) BB",wind_profile(1,0,0),u(1,1,1)
                                    print *, "OCEAN API: gmcfInterpolateWindprofileSpaceOcean(u,v,w) EE",wind_profile(1,ATMOSPHERE_SUB_IP-1,ATMOSPHERE_SUB_JP-1),u(OCEAN_IP,OCEAN_JP,1)
                            end select
                            call gmcfHasPackets(ocean_id,RESPDATA,has_packets)
                        end do
                        print *, "OCEAN API: SANITY: ",sum(wind_profile)
#ifdef GMCF_VERBOSE
                        print *, "OCEAN API: DONE reading DRESP into vars, ready to compute ..."
#endif
                    case default
                        print *, "OCEAN API: current_sync_config = ",current_sync_config," => NO ACTION"
                    ! probably need a default "do nothing" here
                end select
        end if ! FIN
        end if ! is_sync_point

    end subroutine gmcfPreOcean
! ----------------------------------------------------------------------------------------------------------------
    subroutine gmcfPostOcean
!        real(kind=4), dimension(128,128), intent(In) :: wind_profile

        if (gmcfStatus(GMCF_ATMOSPHERE_ID) /= FIN) then
        ! Wait for one post data request
#ifdef GMCF_VERBOSE
            print *,"OCEAN API: POST SHOULD BE OBSOLETE"
#endif


        end if ! FIN
    end subroutine gmcfPostOcean
! ----------------------------------------------------------------------------------------------------------------
    ! The wind_profile from the atmosphere model must be interpolated in space as well as in time
    ! In this case it is best to do the time interpolation first, as that is on smaller arrays
    ! But that is not always the case
    subroutine gmcfInterpolateWindprofileTimeOcean(n_ticks)
        integer, intent(In) :: n_ticks
        integer :: k,jj,ii
        do jj=0,ATMOSPHERE_JP-1
            do ii=0,ATMOSPHERE_IP-1
                do k = 1,3
                    wind_profile_current(k,jj,ii) = gmcfInterpolateTime( wind_profile(k,jj,ii), wind_profile_prev(k,jj,ii), OCEAN_SYNC_PERIOD, n_ticks - n_ticks_at_sync)
                end do
            end do
        end do
    end subroutine gmcfInterpolateWindprofileTimeOcean
! ----------------------------------------------------------------------------------------------------------------
    ! Once we have the time-interpolated values we must interpolate over space, for that I should probably use the third-party routines
    subroutine gmcfInterpolateWindprofileSpaceOcean(u,v,w)
! The values of OCEAN_OCEAN_IP,OCEAN_OCEAN_JP,OCEAN_OCEAN_KP must be known at code generation time!
        real(kind=4), dimension(OCEAN_IP,OCEAN_JP,OCEAN_KP), intent(InOut)  :: u
        real(kind=4), dimension(OCEAN_IP,OCEAN_JP,OCEAN_KP), intent(InOut)  :: v
        real(kind=4), dimension(OCEAN_IP,OCEAN_JP,OCEAN_KP), intent(InOut)  :: w
#if GMCF_INTERPOL_SPACE
        real(4), dimension(4) :: grid_wp = (/ ATMOSPHERE_DI,ATMOSPHERE_DJ,ATMOSPHERE_IMS,ATMOSPHERE_JMS /)
        real(4), dimension(4) :: grid_uvw = (/ OCEAN_DI,OCEAN_DJ,OCEAN_IMS,OCEAN_JMS /)

        call gmcf2DInterpolation(wind_profile(1,:,:),shape( wind_profile(1,:,:) ) ,grid_wp,u(:,:,1),shape( u(:,:,1) ),grid_uvw)
        call gmcf2DInterpolation(wind_profile(2,:,:),shape( wind_profile(2,:,:) ) ,grid_wp,v(:,:,1),shape(v(:,:,1)),grid_uvw)
        call gmcf2DInterpolation(wind_profile(3,:,:),shape( wind_profile(3,:,:) ) ,grid_wp,w(:,:,1),shape(w(:,:,1)),grid_uvw)
#else
        ! TEST 1
!        u(1,1,1) = wind_profile(1,0,0)
!        v(1,1,1) = wind_profile(2,0,0)
!        w(1,1,1) = wind_profile(3,0,0)
!        ! TEST 2
        u(:,:,1) = wind_profile(1,:,:)
        v(:,:,1) = wind_profile(2,:,:)
        w(:,:,1) = wind_profile(3,:,:)
#endif
    end subroutine gmcfInterpolateWindprofileSpaceOcean
! ----------------------------------------------------------------------------------------------------------------
    ! The temperature array to be sent to the atmosphere model is sampled from the original temperature array in the ocean model
    subroutine gmcfSampleTemperatureOcean(t_ocean)
        real(kind=4), dimension(OCEAN_IP,OCEAN_JP), intent(In)  :: t_ocean
        ! In general, sampling requires interpolation
#if GMCF_INTERPOL_SPACE
        #error "TODO"
#else
        ! TEST 1

!        temperature(0,0)=t_ocean(1,1)
!        temperature(0:,0:) = t_ocean(1:,1:)
        temperature = t_ocean
        print *, "gmcfSampleTemperatureOcean:", t_ocean(1,1), '=',temperature(0,0),';',t_ocean(OCEAN_IP,OCEAN_JP),temperature(ATMOSPHERE_SUB_IP-1,ATMOSPHERE_SUB_JP-1)
#endif
    end subroutine gmcfSampleTemperatureOcean
! ----------------------------------------------------------------------------------------------------------------
    subroutine gmcfFinishedOcean
        call gmcfFinished(ocean_id)
    end subroutine gmcfFinishedOcean

end module gmcfAPIocean
