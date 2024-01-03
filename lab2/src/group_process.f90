! Copyright 2015 Fyodorov S. A.

module Group_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Group_IO

   implicit none
   
contains

     ! Получение списков по прописке.
   pure subroutine Get_list_by_reg(Surnames, Initials, Genders, Registration, &
         Reg_Surnames, Aver_Marks, Reg_Initials, Reg_Gender, Reg_Aver_Marks, Reg)
      character(kind=CH_)  Surnames(:, :), Initials(:, :), Genders(:)
      character(kind=CH_)  Reg_Surnames(:, :), Reg_Initials(:, :),Registration(:)
      real(R_)             Reg_Aver_Marks(:), Aver_Marks(:)
      character(kind=CH_)  Reg, Reg_Gender(:)
      intent(in)           Surnames, Initials, Genders, Registration, Reg, Aver_Marks
      intent(out)          Reg_Surnames, Reg_Initials, Reg_Gender, Reg_Aver_Marks
      allocatable          Reg_Surnames, Reg_Initials, Reg_Gender, Reg_Aver_Marks

      logical, allocatable :: Is_A_Reg(:)
      integer, allocatable :: Reg_Pos(:)
      integer              :: Reg_Amount, i
      integer, parameter   :: INDEXES(*) = [(i, i = 1, PEOPLE_AMOUNT)]

      ! Составление логической маски, соответствующей прописки.
      Is_A_Reg    = Registration  == Reg
      Reg_Amount  = Count(Is_A_Reg)

      ! Получение массивов, связынных с заданной пропиской.
      Reg_Pos  = Pack(INDEXES, Is_A_Reg)
      allocate (Reg_Surnames(SURNAME_LEN, Reg_Amount), &
      ! Получение двумерных списков по прописке.
      do concurrent (i = 1:Reg_Amount)
         Reg_Surnames(:, i)  = Surnames(:, Reg_Pos(i))
         Reg_Initials(:, i)  = Initials(:, Reg_Pos(i))
         Reg_Gender(i)       = Genders(Reg_Pos(i))
         Reg_Aver_Marks(i)   = Aver_Marks(Reg_Pos(i))
      end do

   end subroutine Get_list_by_reg

  ! Сортировка списка по среднему баллу.
   pure subroutine Sort_people_list(Surnames, Initials, Genders, Aver_Marks)
      character(kind=CH_)  Surnames(:, :), Initials(:, :), Genders(:)
      real(R_)             Aver_Marks(:)
      intent (inout)       Surnames, Initials,Genders , Aver_Marks

      integer              i, j

      ! Сортировка списка по среднему баллу методом пузырька.
      do i = Size(Aver_Marks), 2, -1
         ! Просматриваем список с начала, ставя в конец менее успешного.
         do j = 1, i-1
            ! Проверка на то, стоит ли менять людей местами.
            if (Swap(Aver_Marks, Surnames, Initials, j)) &
               call Swap_from_current(Surnames, Initials, Genders, Aver_Marks, j)
         end do
      end do
   end subroutine Sort_people_list

  ! Проверка того, стоит ли менять местами текущего челшовека со следующим.
   pure logical function Swap(Aver_Marks, Surnames, Initials, j)
      character(kind=CH_)  Surnames(:, :), Initials(:, :)
      real(R_)             Aver_Marks(:)
      integer              j
      intent (in) :: Surnames, Initials, Aver_Marks, j

      Swap = .false.
      if (Aver_Marks(j) < Aver_Marks(j+1)) then
         Swap = .true.
      else if (Aver_Marks(j) == Aver_Marks(j+1)) then
         if (GT(Surnames(:, j), Surnames(:, j+1))) then
            Swap = .true.
         else if (All(Surnames(:, j) == Surnames(:, j+1)) .and. GT(Initials(:, j), Initials(:, j+1))) then
            Swap = .true.
         end if
      end if
   end function Swap

  ! Функция операции > для массивов символов. Сортировка по алфавиту
   pure logical function GT(arr1, arr2)
      character(kind=CH_), intent(in) :: arr1(:), arr2(:)

      integer :: i

      ! Поиск первого отличного символа или остановка на последнем символе.
      do i = 1, Min(Size(arr1), Size(arr2)) - 1
         if (arr1(i) /= arr2(i)) &
            exit
      end do
      GT = arr1(i) > arr2(i)
   end function GT

     ! Перестановка местами двух эелементов списка, начиная с текущего.
   pure subroutine Swap_from_current(Surnames, Initials, Genders, Aver_Marks, j)
      character(kind=CH_)     Surnames(:, :), Initials(:, :), Genders(:)
      real(R_)                Aver_Marks(:)
      integer, intent(in)  :: j
      intent (inout)       :: Surnames, Initials, Genders, Aver_Marks

      character(kind=CH_)  tmpSurname(SURNAME_LEN), tmpInitials(INITIALS_LEN), tmpGenders
      real(R_)             tmpAverMark

      tmpSurname = Surnames(:, j+1)
      Surnames(:, j+1) = Surnames(:, j)
      Surnames(:, j) = tmpSurname

      tmpInitials = Initials(:, j+1)
      Initials(:, j+1) = Initials(:, j)
      Initials(j, :) = tmpInitials

      !tmpSurname = Surnames(:, j+1)
      !Surnames(:,j+1) = Surnames(:, j)
      !Surnames(:,j) = tmpSurname

      tmpGenders = Genders(j+1)
      Genders(j+1) = Genders(j)
      Genders(j) = tmpGenders

      tmpAverMark = Aver_Marks(j+1)
      Aver_Marks(j+1) = Aver_Marks(j)
      Aver_Marks(j) = tmpAverMark
   end subroutine Swap_from_current
  
    ! Сортировка списка класса по среднему баллу.
  ! pure subroutine Sort_class_list(Group)
  !    type(student), intent(inout)  :: Group(:)

  !    integer        :: i, j
  !    type(student)  :: tmp_stud

  !    ! Сортировка списка класса по среднему баллу методом пузырька.
  !    do i = Size(Group), 2, -1
  !       ! Просматриваем список с начала, ставя в конец менее успешного.
  !       do j = 1, i-1
  !          ! Проверка на то, стоит ли менять учащихся местами.
  !          if (Swap(Group, j)) then
  !             ! Перестановка местами двух эелементов списка, начиная с текущего.
  !             tmp_stud = Group(j+1)
  !             Group(j+1) = Group(j)
  !             Group(j) = tmp_stud
  ! 		   ! Group(j:j+1) = Group(j+1:j:-1)
  !          end if
  !       end do
  !    end do
  ! end subroutine Sort_class_list

  ! ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
  ! pure logical function Swap(Group, j)
  !    type(student), intent(in)  :: Group(:)
  !    integer, intent(in)        :: j

  !    Swap = .false.
  !    if (Group(j)%Aver_mark < Group(j+1)%Aver_mark) then
  !       Swap = .true.
  !    else if (Group(j)%Aver_mark == Group(j+1)%Aver_mark) then
  !       if (Group(j)%Surname > Group(j+1)%Surname) then
  !          Swap = .true.
  !       else if (Group(j)%Surname==Group(j+1)%Surname .and. Group(j)%Initials>Group(j+1)%Initials) then
  !          Swap = .true.
  !       end if
  !    end if
  ! end function Swap
end module group_process
