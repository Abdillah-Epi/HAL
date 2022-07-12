##
## EPITECH PROJECT, 2020
## B-FUN-501-RUN-5-1-HAL-abdillah-kael.madi
## File description:
## Makefile
##

NAME=		hal

STACK=		stack
MV=			mv
RM=			rm -f

all:
			${STACK} build
			${MV} $(shell ${STACK} path --local-install-root)/bin/HAL-exe hal

clean:
			${STACK} clean

fclean:		clean
			$(RM) $(NAME)

re:			fclean all

.PHONY:		all clean fclean re