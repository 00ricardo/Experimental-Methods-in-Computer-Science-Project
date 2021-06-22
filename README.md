# Experimental-Methods-in-Computer-Science-Project
Experimental Methods in Computer Science-Project

## Identificação de variáveis para o estudo
Tendo em conta que se pretende medir o desempenho de um sistema computacional, e
sendo este uma avaliação muito abstrata, passamos a avaliar o sistema através de uma
variável denominada “Total Wait Time”, resultante da soma do tempo em fila de espera do CPU
e do I/O. Resumidamente, Total Wait Time = CPU Wait Time + I/O Wait Time, que será a
quantidade de tempo que um processo fica em fila de espera aguardando pelo processamento.
Por fim, o tipo de variável nível no nosso estudo experimental tomará os valores
correspondentes aos algoritmos de escalonamento, First Come First Served, Round-Robin,
Shortest Job First, Shortest Remaining Time First, ou seja, “FCFS”, “RR”, “SJF”, “SRTF”,
respectivamente. Ainda relativamente ao algoritmo Round-Robin, este utiliza uma variável
denominada Quantum que determina o tempo de processamento de cada burst real de CPU,
contudo irá permanecer intacta em toda a experiência..
Com base nos requisitos do trabalho, de modo a fazer um estudo inferencial, decidimos
então alterar, não só o cenário de estudo, mas também as variáveis que queremos avaliar.
Dependendo do tipo e objetivo de estudo as variáveis desempenham um papel específico. No
caso do estudo de comparações entre algoritmos (T-Test e ANOVA) a variável dependente em
ambas técnicas foi o Total Wait Time em função dos grupos (algoritmos), que seria a nossa
variável independente. No caso do estudo de escalabilidade, a variável dependente é o Total
Wait Time e a variável independente é o número de processos.

## Formulação do cenário e de hipóteses experimentais
Foi pensado um cenário melhorado em relação à primeira meta, onde idealizamos um
servidor real de Jogos de Xadrez. Sendo que cada processo do servidor será um jogo de
xadrez a decorrer, e o CPU Burst e o IO Burst serão as jogadas do sistema(Inteligência
Artificial) e do jogador, respetivamente.
Com base neste cenário foram criados vários workloads onde o número médio de
jogadas é 40 (número médio de jogadas de um jogo de xadrez, segundo a Lei de Shannon), as
jogadas do jogador variam entre 5 e 10 unidades de tempo, e as jogadas do sistema variam
entre 4 e 6 nas primeiras 10/12 jogadas e últimas 5, sendo que as jogadas intermédias o tempo
é multiplicado por um valor entre 4 a 6, assemelhando-se a uma distribuição normal. Para se
analisar o impacto do aumento de número processos no sistema foram criados 8 workloads
diferentes com 10, 30, 40, 50, 60, 150, 500 e 1000 números de processos respectivamente.
