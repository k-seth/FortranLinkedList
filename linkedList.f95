! Author: Seth Kuipers
! Due: Feb 1, 2019
! Course: CIS*3190
! Project: Assignment 1

! A module containing the api to construct and load a linked list
! It also contains the subroutines for add/subtract/multiply and factorial
! I debated putting these in as part of the regular program, but deced to keep all list iterating material together
module list_api
    implicit none

    ! a type def to define an internal node fror the list
    ! Contains a field to store the number, and a pointer to the next and previous nodes
    type linked_list
        integer :: num
        type (linked_list), pointer :: next => null()
        type (linked_list), pointer :: previous => null()
    end type linked_list

    ! a type def to represent a pointer to a list structure
    ! This should only exist once per list, and contains the front and end of the list
    ! Semanticly speaking, the two are lists are technivally identical in their fields (that is mostly a result of changes during development), but they can and often are different. This is just an odd ball case
    type list_head
        integer :: isNegative
        type (linked_list), pointer :: front => null()
        type (linked_list), pointer :: tail => null()
    end type list_head

    contains

    ! A simple subroutine to iterate over a list and print all values
    subroutine printList(list)
        type (list_head), target :: list
        type (linked_list), pointer :: current

        ! In case the value was identified as negative, print the symbol
        if(list%isNegative == 1) then
            write(*,fmt="(a)", advance="no") "-"
        end if

        current => list%front

        ! Iterate through the list and print it
        do while (associated(current))
            write(*,fmt="(i1)", advance="no") current%num
            current => current%next
        end do
        ! Add an extra line at the end
        write(*,*)
    end subroutine printList

    ! Builds/inserts a linked list in a top down style. That is to say that Nodes are always inserted at the back of the list
    subroutine insertBack(head, str)
        type (list_head), target :: head
        type (linked_list), pointer :: node

        character, intent (in) :: str
        integer :: number

        if(str == "-") then
            head%isNegative = 1
            return
        end if

        ! Convert the read in character to an integer, and then assign it the node
        read(str, "(i1)") number

        allocate(node)
        node%num = number

        ! Check to see if the list is empty or not. If it is, assign front and tail to the node
        ! Otherwise, just insert it to the tail of the list
        if(.not. associated(head%front)) then
            head%front => node
        else
            node%previous => head%tail
            head%tail%next => node
        end if
        head%tail => node
    end subroutine insertBack

    ! Builds/inserts a linked list in a bottom up style. That is to say that Nodes are always inserted at the front of the list
    subroutine insertFront(head, number)
        type (list_head), target :: head
        type (linked_list), pointer :: node

        integer :: number

        allocate(node)

        node%num = number

        ! Check to see if the list is empty or not. If it is, assign front and tail to the node
        ! Otherwise, just insert it to the tail of the list
        if(.not. associated(head%front)) then
            head%tail => node
        else
            head%front%previous => node
            node%next => head%front
        end if
        head%front => node
    end subroutine insertFront

    ! Handles adding two lists together
    subroutine add(listOne, listTwo, answer)
        type (list_head), target :: listOne
        type (list_head), target :: listTwo
        type (list_head), target :: answer

        type (linked_list), pointer :: nodeOne
        type (linked_list), pointer :: nodeTwo

        integer :: overflow
        integer :: value
        integer :: total

        nodeOne => listOne%tail
        nodeTwo => listTwo%tail

        overflow = 0 ! As noted in another function, doing this in the initializer did not work. I am not sure why, clearly I am missing something
        ! Addendum to above comment
        ! Okay, so I did a bit of hunting and it turns out that is a bit of a gotcha with Fortran
        ! Fortran will save variable state across function calls if they have the SAVE attribute
        ! Turns out, any variable initialized at the same time as declaration is given this attribute, which I was not aware of
        ! Only in certain cases (depending on the order the variables were passed in) did an issue arise, however
        ! For example, 13! was 1 off (all values before this were fine. That was confusing, for sure), and 13 * 479001600 was also off by 1. But, 479001600 * 13 was CORRECT. So, odd, and evidently a result of how the multiply subroutine wors
        ! So, turns out pre-initialized variables are potentially dangerous and could lead to unintentional bleed through of numbers
        ! Interesting learning chance to say the least
        do while (associated(nodeOne) .or. associated(nodeTwo))
            ! Set up the tracking of the value to go in the node
            total = 0
            total = total + overflow
            overflow = 0

            if (associated(nodeOne)) then
                total = total + nodeOne%num
                nodeOne => nodeOne%previous
            end if
            if (associated(nodeTwo)) then
                total = total + nodeTwo%num
                nodeTwo => nodeTwo%previous
            end if

            value = modulo(total, 10)
            overflow = total/10

            call insertFront(answer, value)
        end do
        if(overflow > 0) then ! Incase there is overflow, make sure to add it when the loop exits
          call insertFront(answer, overflow)
        end if
    end subroutine add

    ! Handles the subtraction of 2 lists
    subroutine subtract(listOne, listTwo, answer)
        type (list_head), target :: listOne
        type (list_head), target :: listTwo
        type (list_head), target :: answer

        type (linked_list), pointer :: nodeOne
        type (linked_list), pointer :: nodeTwo

        integer :: number

        nodeOne => listOne%tail
        nodeTwo => listTwo%tail
        
        ! Construct te new answer
        do while (associated(nodeOne) .or. associated(nodeTwo))
            number = 0

            ! If both lists are valid and exist, subtract them from each other
            ! if only one exists, then by default that becomes the value of the answer node
            if(associated(nodeOne) .and. associated(nodeTwo)) then
                if(nodeOne%num < nodeTwo%num) then 
                    ! Removes one from the next node, if it exists, and adds 10 to the current node
                    ! If the next node is null, then num is set to 0
                    if(associated(nodeOne%previous)) then
                        nodeOne%previous%num = nodeOne%previous%num - 1
                        nodeOne%num = nodeOne%num + 10
                    else if((.not. associated(nodeOne%previous))) then
                        nodeOne%num = 0
                    end if
                end if

                number = nodeOne%num - nodeTwo%num
                if(number < 0) then
                    number = number * (-1)
                end if

                nodeOne => nodeOne%previous
                nodeTwo => nodeTwo%previous
            else ! listOne is longer than listTwo
                if(nodeOne%num == 0) then
                    return
                end if
              
                number = nodeOne%num
                nodeOne => nodeOne%previous
            end if

            call insertFront(answer, number)
        end do
    end subroutine subtract

    ! This is needed because the greater value must be on top when doing subtraction
    ! 1 is returned for list one being greater (either longer or larger value), otherwise 2 is returned
    subroutine compare_lists(listOne, listTwo, greater)
        type (list_head), target :: listOne
        type (list_head), target :: listTwo
        integer, intent(out) :: greater

        type (linked_list), pointer :: nodeOne
        type (linked_list), pointer :: nodeTwo

        nodeOne => listOne%front
        nodeTwo => listTwo%front

        ! Check length first
        do while (associated(nodeOne) .and. associated(nodeTwo))
            if(.not. associated(nodeOne%next) .and. (.not. associated(nodeTwo%next))) then ! make sure to check if both are invalid first, becuase then they are the same length and that is okay
                exit
            else if(.not. associated(nodeOne%next)) then
                greater = 2
                return
            else if(.not. associated(nodeTwo%next)) then
                greater = 1
                return
            else
                nodeOne => nodeOne%next
                nodeTwo => nodeTwo%next
            end if
        end do  

        nodeOne => listOne%front
        nodeTwo => listTwo%front

        do while (associated(nodeOne) .and. associated(nodeTwo))
            if(nodeOne%num - nodeTwo%num > 0) then
                greater = 1
                return
            else if(nodeOne%num - nodeTwo%num < 0) then
                greater = 2
                return
            else
                nodeOne => nodeOne%next
                nodeTwo => nodeTwo%next
            end if
        end do
        greater = 1 ! In the case they are identical, just say the first one is
    end subroutine compare_lists

    ! Handles multiplying 2 lists
    subroutine multiply(listOne, listTwo, answer)
        type (list_head), target :: listOne
        type (list_head), target :: listTwo
        type (list_head), pointer :: answer

        type (list_head), pointer :: tempListOne
        type (list_head), pointer :: tempListTwo

        type (linked_list), pointer :: nodeOne
        type (linked_list), pointer :: nodeTwo

        integer :: total
        integer :: overflow
        integer :: value
        integer :: offset
        integer :: i

        offset = 0 ! *RESOLVED* This was initially set on initialization, but for whatever reason it didn't work. Repeated calls from factorial kept the previous call's offset value. So, explicitly reset
        overflow = 0 

        nodeTwo => listTwo%tail

        ! This loops through all digits of the second list
        do while(associated(nodeTwo))
            nodeOne => listOne%tail
            allocate(tempListOne)
            allocate(tempListTwo)
            do i=1, offset ! Offset is needed when increasing the 10s value of the second list
                call insertFront(tempListOne, 0)
            end do

            overflow = 0
            do while(associated(nodeOne)) ! This iterates over every element of the first list for the current node in the second list
                total = 0
                total = total + overflow
                total = total + (nodeTwo%num * nodeOne%num)
                overflow = 0
                if(total > 9) then ! This means we need to use two nodes
                    value = modulo(total, 10)
                    overflow = total / 10
                else 
                    value = total
                end if

                call insertFront(tempListOne, value) ! Insert the value for the single node
                nodeOne => nodeOne%previous
            end do

            if(overflow > 0) then ! Again, in case the last existing node was > 9, create one last new one to hold remainder
                call insertFront(tempListOne, overflow)
            end if

            call add(tempListOne, answer, tempListTwo) ! Add the new list with the answer list, saving to a temp

            call freeList(answer)
            answer => tempListTwo ! Reassign the answer list to the temp one so we can go again
            call freeList(tempListOne)
            offset = offset + 1 
            nodeTwo => nodeTwo%previous
        end do
    end subroutine multiply

    ! Handles factorial of a list
    subroutine factorial(listOne, answer)
        type (list_head), target :: listOne
        type (list_head), pointer :: answer

        type (list_head), pointer :: tempListOne
        type (list_head), pointer :: tempListTwo

        type (linked_list), pointer :: nodeOne

        integer :: factorialNum=0 ! Since factorial can only ever be called once, I can get away with this
        integer :: counter=0
        integer :: temp=0
        nodeOne => listOne%front

        ! This is the node for 0!. We will assume this is already in for this
        call insertFront(answer, 1)

        ! Because the assignment specifies that the value for this will be a small number, it will fit in a standard integer
        ! As such, go through and construct the number
        do while (associated(nodeOne))
            factorialNum = factorialNum * 10
            factorialNum = factorialNum + nodeOne%num
            
            nodeOne => nodeOne%next
        end do

        do while (factorialNum /= counter) ! Run until the input value is equal to the counter
            counter = counter + 1

            allocate(tempListOne)
            temp = counter
            do while (counter /= 0) ! Break the counter into a list
                call insertFront(tempListOne, modulo(counter,10))
                counter = counter / 10
            end do
            counter = temp

            allocate(tempListTwo)
            call multiply(tempListOne, answer, tempListTwo)

            call freeList(answer)
            answer => tempListTwo
            call freeList(tempListOne)            
        end do

    end subroutine factorial

    ! Cleans up all memory associated with a linked list, including the head list item (hence why it is a pointer, not a target here)
    subroutine freeList(list)
        type (list_head), pointer :: list
        type (linked_list), pointer :: curr
        type (linked_list), pointer :: delete

        curr => list%front

        do while(associated(curr))
            delete => curr
            curr => curr%next
            deallocate(delete)
        end do

        deallocate(list)
    end subroutine freeList
end module list_api

program linkedList
    use list_api
    implicit none
    
    type (list_head), pointer :: listOne
    type (list_head), pointer :: listTwo
    type (list_head), pointer :: answer

    integer, parameter :: n=100
    integer :: i
    integer :: greater

    character (len = 1) :: op
    character (len = n) :: numOne, numTwo

    ! Only allow a valid operation
    do
        write(*,*) "Enter an operation: +, -, * or !"
        read (*,*) op
        if(op == "+" .or. op == "-" .or. op == "*" .or. op == "!") then
            exit
        end if
    end do

    ! These would be tough to check for validity, so I won't try
    ! I will assume user input here is clean 
    write(*,*) "Enter first operand:"
    read (*,*) numOne

    allocate(listOne)
    listOne%isNegative = 0
    do i = 1,len(trim(numOne))
        call insertBack(listOne, numOne(i:i))
    end do


    if (op /= "!") then
        write(*,*) "Enter second operand:" ! Since factorial only uses one, this only occurs for the other 3
        read (*,*) numTwo

        allocate(listTwo)
        listTwo%isNegative = 0
        do i = 1,len(trim(numTwo))
            call insertBack(listTwo, numTwo(i:i))
        end do
    end if

    ! Make space for all the lists
    allocate(answer)
    answer%isNegative = 0

    ! Do sign "change" for subtraction
    if(op == "-" .and. listTwo%isNegative == 1) then
        listTwo%isNegative = 0    
    else if(op == "-" .and. listTwo%isNegative == 0) then
        listTwo%isNegative = 1
    end if

    ! If the user gives two positives or two negatives to add, then just add them as it is easy
    ! If they supply opposite signs for subtraction, again, just add, because it is the same as adding two similar ones
    ! However, if they give opposite signs for add or same signs for subtract, then subtration must be applied

    ! Because of how much was going on in the subroutines in the linked_list implementation, I decided to handle the positive/negative handling here
    ! Not the cleanest implementation, but most of them had slightly different conditions that would have been extra/unnecessary checks for the other operations which may have messed up the signage
    if((op == "+" .or. op == "-") .and. listOne%isNegative == listTwo%isNegative) then
        call add(listOne, listTwo, answer)
        if(listOne%isNegative == 1 .and. listTwo%isNegative == 1) then
            answer%isNegative = 1
        end if
    else if((op == "+" .or. op == "-") .and. listOne%isNegative /= listTwo%isNegative) then 
        call compare_lists(listOne, listTwo, greater)
        if(greater == 1) then
            call subtract(listOne, listTwo, answer)
            answer%isNegative = listOne%isNegative
        else
            call subtract(listTwo, listOne, answer)
            answer%isNegative = listTwo%isNegative
        end if
    else if(op == "*") then ! Multiply
        call multiply(listOne, listTwo, answer)
        if(listOne%isNegative /= listTwo%isNegative) then
            answer%isNegative = 1
        end if
    else
        call factorial(listOne, answer)
        answer%isNegative = listOne % isNegative
    end if

    write(*,*) "Result is:"
    call printList(answer)

    call freeList(listOne)
    if (op /= "!") then
        call freeList(listTwo)
    end if
    call freeList(answer)
end
