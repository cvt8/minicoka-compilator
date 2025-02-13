my_malloc:
    pushq %rbp
    movq %rsp, %rbp
    andq $-16, %rsp # alignement de la pile
    movq 24(%rbp), %rdi # argument de malloc ici passé sur la pile
    call malloc
    movq %rbp, %rsp
    popq %rbp
    ret
