# Bolao_Brasileirao_2020
This is the folder contains the files used to develop a platform for a betting game on brazilian soccer league "Campeonato Brasileiro 2020".

The platform can be acessed through https://mpcambrainha.shinyapps.io/bolao/

<b>Disclaimer</b>: This is a not a professional product, since this platform was intended only to be a for fun platform in which the target audience are friends with over a decade of friendship. Most of the language used is informal and the focus of the platform relies in functionality, not design. Also, the language used is portuguese since all participants are brazilian.

## The Game:
The betting game, "Bolão", for which this is platform was designed for, consists in each player guessing the results of soccer matches played in "Campeonato Brasileiro 2020". For each guess, the player gets points based on how close the guess was to the real result. In each round of the game, players are assigned to play against each other and the player beats his opponent if he scores 10 or more points thans his opponent's score.

In the first phase, all players play against each other twice. After 26 rounds, the players are assigned to three subgroups, based on the final standings.
* **Grupo A**: players in positions 1, 4,5 and 8 proceed to this group and each player will face the other three twice.
* **Grupo B**: players in position 2, 3, 6 and 7 proceed to this group and each player will face the other three twice.
* **Hexagonal do Rebaixamento**: players in position 9 through 14 proceed to this group and each player will face the other five once.

Afters this rounds, the first and second player of Grupo A and Grupo B qualify to the final playoff stage.

## The platform
The platform used contains a login system, so each player could acess with his own credentials and send his guesses. Also, there's a guest credential for non-players to acess the platform.

* **Nome de Usuário**: observador
* **Senha**: cafecomleite

The platform contains commands to recognize the time in which the matches start and close the system for sending guesses. Since the game is already over, the system only shows the state in which the matches are already underway. To check the state in which you could submit guesses, acess the app with the following credentials:

* **Nome de Usuário**: placar_real
* **Senha**: placar_real

In the guessing page, the user can submit his guesses. After clicking "Enviar", the guesses are sent to an e-mail account and the code will automatically acess these e-mail, to present the guess of which player, when the round starts.

The platform other functionalities could be acessed through the tabs in the right side of the page. Each tab is explained as follows:

* **Regulamento**: This page contains the pdf file with the detailed rules of the game.
* **Palpites**: This is a page with two states. When the games are not underway, this page is used by the users to send their guesses. When the games are under way, this page shows the guesses for each battle between two players.
* **Confrontos**: This page shows the result of previous battles and the upcoming battles.
* **Palpite anterior**: This page shows the user recorded last guess sent.
* **Classificação**: The page contains the tables with standings for first (divided in "Geral", "1º turno" and "2º turno") and second phase. Also contains tables with standings based guessing points ("Pontos Pró"), right guesses ("Cravadas"), guesses about the league champion and top scorer ("Bônus") and form ("Sequência"). 
* **Parciais**: This page shows the results of soccer matches completed and underway. Also it presents the partials results of the game battles between players.
* **Gráficos**: The page contains varioues graphs presenting statistics of the game for each player and their relation with the league soccer teams.
* **Enviados**: This page shows which players have already sent their guesses for the round.
* **Histórico**: This page show results of the each possible battle for the previous editions of the game.
