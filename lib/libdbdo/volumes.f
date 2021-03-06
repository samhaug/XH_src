      subroutine volumes()
      include 'seedparam.h'
      include 'seedcommun.h'
      include 'seedtrees.h'
      logical getabr
      integer trread
      character*60 name
      character*100 path,path1,blpath
      integer*4 iname(15)
      equivalence (name,iname)

      idblf=1
      do while (getabr(itblabr,idblf,tmpblk,ltmp))
c       write(6,'(i8,a)') idblf,'-- '//tmpblk(1:ltmp)
        lud=-1
        call opnflc(lud,tmpblk(1:ltmp),1,0,0,istbl,-1,0)
        if(istbl.eq.0) then
          call closfl(lud,istat)
        else
          write(6,*) tmpblk(1:ltmp)//' *** EAT FILE NOT FOUND'
        endif

        nw=1
        knt=0
        path=' '
        lpath=0
        do while(nw.gt.0)
          nw=trread(itblabr,iname,15)
          if(nw.gt.0) then
            knt=knt+1
            lname=istlen(name(1:4*nw))
c           write(6,'(8x,a)') '   '//name(1:lname)
            if(lname.gt.5.and.name(1:5).eq.'/opt/') then
              lud=-1
              call opnflc(lud,'/home/seiraid1/j1/'//name(6:lname),1,0,0,ist1,-1,0)
              if(ist1.eq.0) then
                call closfl(lud,istat)
                path='/home/seiraid1/j1/'//name(6:lname)
                lpath=istlen(path)
              else
                lud=-1
                call opnflc(lud,'/home/seiraid2/j2/'//name(6:lname),1,0,0,ist2,-1,0)
                if(ist2.eq.0) then
                  call closfl(lud,istat)
                  path='/home/seiraid2/j2/'//name(6:lname)
                  lpath=istlen(path)
                endif
              endif
            endif
          endif
        enddo
        if(lpath.le.0.and.knt.gt.0) write(6,*) tmpblk(1:ltmp)//' *** DATA NOT FOUND knt=',knt

        if(lpath.gt.0.and.tmpblk(ltmp-2:ltmp).eq.'.bl') then
          ll=lpath
          do while (ll.gt.0.and.path(ll:ll).ne.'/')
            ll=ll-1
          enddo
          if(ll.le.0.or.path(ll:ll).ne.'/') pause 'volumes: error 1'
          path1=path(1:ll)//''''//path(ll+1:lpath)//''''
          lpath1=istlen(path1)

          ll=ltmp
          do while (ll.gt.0.and.tmpblk(ll:ll).ne.'/')
            ll=ll-1
          enddo
          if(ll.le.0.or.tmpblk(ll:ll).ne.'/') pause 'volumes: error 2'
          blpath=tmpblk(1:ll)//''''//tmpblk(ll+1:ltmp)//''''
          lblpath=istlen(blpath)
          write(177,'(a)') 'ln -s '//path1(1:lpath1)//' '//blpath(1:lblpath-4)//'.se'''
          write(178,'(a)') 'eat '//blpath(1:lblpath)//' /home/users/john/ar/'//blpath(1:lblpath-4)//'.se'''
        endif
        idblf=idblf+1
      enddo
      return
      end
