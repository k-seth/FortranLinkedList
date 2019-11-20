! Author: Seth Kuipers
! Course: CIS*3190
! Project: Assignment 4

! A module containing the definitions for defining and constructing a linked list
module linked_list
    implicit none

    ! Defines a single node for the linked list. This should not be used to represent an entire list
    ! Contains a field to store the number, and a pointer to the next and previous nodes
    type list_node
        integer :: num
        type (list_node), pointer :: next => null()
        type (list_node), pointer :: previous => null()
    end type list_node

    ! Defines a pointer to a list which contains the head and tail of the list. This should only exist once per list and not as a node
    ! Semantically speaking, the two are lists are technically identical in their fields, but they can be different based on implementation
    type list_pointer
        integer :: isNegative
        type (list_node), pointer :: head => null()
        type (list_node), pointer :: tail => null()
    end type list_pointer

    contains

    ! A simple subroutine to iterate over a list and print all values
    subroutine printList(list)
        type (list_pointer), target :: list
        type (list_node), pointer :: current

        ! In case the value was identified as negative, print the symbol
        if(list%isNegative == 1) then
            write(*,fmt="(a)", advance="no") "-"
        end if

        current => list%head

        ! Iterate through the list and print it all on one line
        do while (associated(current))
            write(*,fmt="(i1)", advance="no") current%num
            current => current%next
        end do
        ! Add an extra line at the end
        write(*,*)
    end subroutine printList

    ! Builds/inserts a linked list in a top down style. That is to say that nodes are always inserted at the back of the list
    subroutine insertBack(list, str)
        type (list_pointer), target :: list
        type (list_node), pointer :: node

        character, intent (in) :: str
        integer :: number

        if(str == "-") then
            list%isNegative = 1
            return
        end if

        ! Convert the read in character to an integer, and then assign it the node
        read(str, "(i1)") number

        allocate(node)
        node%num = number

        ! Check to see if the list is empty or not. If it is, assign head and tail to the node. Otherwise, just insert it to the tail of the list
        if(.not. associated(list%head)) then
            list%head => node
        else
            node%previous => list%tail
            list%tail%next => node
        end if
        list%tail => node
    end subroutine insertBack

    ! Builds/inserts a linked list in a bottom up style. That is to say that Nodes are always inserted at the head of the list
    subroutine insertFront(list, number)
        type (list_pointer), target :: list
        type (list_node), pointer :: node

        integer :: number

        allocate(node)

        node%num = number

        ! Check to see if the list is empty or not. If it is, assign head and tail to the node
        ! Otherwise, just insert it to the tail of the list
        if(.not. associated(list%head)) then
            list%tail => node
        else
            list%head%previous => node
            node%next => list%head
        end if
        list%head => node
    end subroutine insertFront

    ! Cleans up all memory associated with a linked list, including the head list item (hence why it is a pointer, not a target here)
    subroutine freeList(list)
        type (list_pointer), pointer :: list
        type (list_node), pointer :: curr
        type (list_node), pointer :: delete

        curr => list%head

        do while(associated(curr))
            delete => curr
            curr => curr%next
            deallocate(delete)
        end do

        deallocate(list)
    end subroutine freeList
end module linked_list

! This module contains the math subroutines. It makes use of the previously defined linked list
module math_lib
    use linked_list
    implicit none

    contains

    ! Handles adding 2 lists together
    subroutine add(listOne, listTwo, answer)
        type (list_pointer), target :: listOne
        type (list_pointer), target :: listTwo
        type (list_pointer), target :: answer

        type (list_node), pointer :: nodeOne
        type (list_node), pointer :: nodeTwo

        integer :: overflow
        integer :: value
        integer :: total

        nodeOne => listOne%tail
        nodeTwo => listTwo%tail

        overflow = 0
        ! Note: This can value can not be set when initialized as the variable gains the SAVE attribute
        ! This would result is the value being persistent across function calls

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
        type (list_pointer), target :: listOne
        type (list_pointer), target :: listTwo
        type (list_pointer), target :: answer

        type (list_node), pointer :: nodeOne
        type (list_node), pointer :: nodeTwo

        integer :: number

        nodeOne => listOne%tail
        nodeTwo => listTwo%tail
        
        ! Construct the new answer
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

    ! Helper function to determine which list is greater for ordering for subtraction
    subroutine compare_lists(listOne, listTwo, greater)
        type (list_pointer), target :: listOne
        type (list_pointer), target :: listTwo
        integer, intent(out) :: greater

        type (list_node), pointer :: nodeOne
        type (list_node), pointer :: nodeTwo

        nodeOne => listOne%head
        nodeTwo => listTwo%head

        ! Check length first
        do while (associated(nodeOne) .and. associated(nodeTwo))
            if(.not. associated(nodeOne%next) .and. (.not. associated(nodeTwo%next))) then ! make sure to check if both are invalid first, because then they are the same length and that is okay
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

        nodeOne => listOne%head
        nodeTwo => listTwo%head

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
        type (list_pointer), target :: listOne
        type (list_pointer), target :: listTwo
        type (list_pointer), pointer :: answer

        type (list_pointer), pointer :: tempListOne
        type (list_pointer), pointer :: tempListTwo

        type (list_node), pointer :: nodeOne
        type (list_node), pointer :: nodeTwo

        integer :: total
        integer :: overflow
        integer :: value
        integer :: offset
        integer :: i

        offset = 0
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

            if(overflow > 0) then ! In case the last existing node was > 9, create one last new one to hold remainder
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

    ! Future plan: Division

    ! Handles factorial of a list
    subroutine factorial(listOne, answer)
        type (list_pointer), target :: listOne
        type (list_pointer), pointer :: answer

        type (list_pointer), pointer :: tempListOne
        type (list_pointer), pointer :: tempListTwo

        type (list_node), pointer :: nodeOne

        integer :: factorialNum=0 ! Since factorial can only ever be called once, I can get away with this
        integer :: counter=0
        integer :: temp=0

        nodeOne => listOne%head

        ! This is the node for 0!. We will assume this is already in
        call insertFront(answer, 1)

        ! It is assumed input will fit in a standard integer
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

end module math_lib

! Main program. Takes user input and constructs linked lists
program linkedList
    use linked_list
    use math_lib
    implicit none
    
    type (list_pointer), pointer :: listOne
    type (list_pointer), pointer :: listTwo
    type (list_pointer), pointer :: answer

    integer, parameter :: n=100
    integer :: i
    integer :: greater

    character (len = 1) :: op
    character (len = n) :: numOne, numTwo

    do
        write(*,*) "Enter an operation: +, -, * or !"
        read (*,*) op
        if(op == "+" .or. op == "-" .or. op == "*" .or. op == "!") then ! Only allow a valid operation
            exit
        end if
    end do

    ! TODO: Check user input
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

        ! Do sign "change" for subtraction
        if(op == "-" .and. listTwo%isNegative == 1) then
            listTwo%isNegative = 0    
        else if(op == "-" .and. listTwo%isNegative == 0) then
            listTwo%isNegative = 1
        end if
    end if

    allocate(answer)
    answer%isNegative = 0

    ! If the user gives two positives or two negatives to add, then just add them as it is easy
    ! If they supply opposite signs for subtraction, again, just add, because it is the same as adding two similar ones
    ! However, if they give opposite signs for add or same signs for subtract, then subtration must be applied

    ! Because of how much was going on in the subroutines in the math_lib implementation, I decided to handle the positive/negative handling here
    ! Not the cleanest implementation, but most of them had slightly different conditions that would have been extra/unnecessary checks for the other operations which may have messed up the signage
    if(op == "!") then ! Factorial - Check first to avoid seg fault due to listTwo
        call factorial(listOne, answer)
        answer%isNegative = listOne%isNegative
    else if((op == "+" .or. op == "-") .and. listOne%isNegative == listTwo%isNegative) then ! Add
        call add(listOne, listTwo, answer)
        if(listOne%isNegative == 1 .and. listTwo%isNegative == 1) then
            answer%isNegative = 1
        end if
    else if((op == "+" .or. op == "-") .and. listOne%isNegative /= listTwo%isNegative) then ! Subtract
        call compare_lists(listOne, listTwo, greater)
        if(greater == 1) then
            call subtract(listOne, listTwo, answer)
            answer%isNegative = listOne%isNegative
        else
            call subtract(listTwo, listOne, answer)
            answer%isNegative = listTwo%isNegative
        end if
    else ! Multiply
        call multiply(listOne, listTwo, answer)
        if(listOne%isNegative /= listTwo%isNegative) then
            answer%isNegative = 1
        end if
    end if

    write(*,*) "Result is:"
    call printList(answer)

    call freeList(listOne)
    if (op /= "!") then
        call freeList(listTwo)
    end if
    call freeList(answer)
end