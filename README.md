# conceitos
Trazendo alguns conceitos dos modelos estatísticos de machine learning, como modelo KNN, Baggning, Ridge e outros

O modelo KNN é um método não paramétrico, isto é, não há premissas acerca os dados a serem utilizados. Ele pode ser utilizado tanto para regressão quanto para classificação. Importante salientar que não é um modelo que aprende com a base de dados, mas cujos resultados derivam dos dados inseridos (lazy learning).
O modelo consiste em calcular o beta mediante a mensuração da distância (Euclidiana) mais próxima de um ponto em relação a k-pontos. Então, por exemplo, se k = 3, o modelo captura os 3 pontos mais próximos ao dado-referência, e calcula a distância ponderada. 
Ou seja, um ponto estará, assim, dentro de uma categoria pertencente aos vizinhos, e associados a um Y (variável independente).
Portanto, se uma nova informação é incluída na base de dados, o algoritmo irá identificar os k pontos próximos, e trazer um valor.
Esta abordagem necessita de 3 elementos:
1. matriz com os preditores associados à base de treino
2. matriz de dados a serem utilizados no erro teste
3. o valor de k, isto é, o número de vizinhos a ser usado como classificador
No entanto, como saber qual k utilizar? Três, cinco, nove ou mais? 
Não existe um método específico para encontrar o valor ótimo de k. Uma medida seria a raiz quadrada do número de elementos da base de dados. Por exemplo, se há 25 dados, k será igual a 5.
Ao final do cálculo de todas as distância, será formada uma linha que as interliga, sendo então possível calcular o erro quadrado médio somando a subtração da diferença entre o ponto y e a linha.
