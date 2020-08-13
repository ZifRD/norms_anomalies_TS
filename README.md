# norms_anomalies_TS

Материалы для статьи с рабочим названием "ИЗМЕНЧИВОСТЬ АНОМАЛИЙ ТЕМПЕРАТУРЫ И СОЛЕНОСТИ ВОД В АВГУСТЕ-СЕНТЯБРЕ НА РАЗРЕЗЕ «КОЛЬСКИЙ МЕРИДИАН» ЗА ПЕРИОД С 1970 ПО 2019 ГГ." (Моисеев Д.В., Запорожцев И.Ф., Максимовская Т.М., план завершения - сентябрь 2020 г.).
Веб-приложение и набор утилит, которые обрабатывают данные World Ocean Database и ICES для обновления референсных значений океанографических характеристик (норм и аномалий) на разрезе "Кольский Меридиан" и построения отчетных изображений для лаб. ОиР ММБИ РАН. 

Далее перечислены файлы, которые содержат сниппеты построения различных изображений по данным, с которыми работает веб-приложение. После генерации изображения были обработаны в Gimp (в частности, могли использоваться несколько сгенерированных изображений в итоговом). 

1. Распределение количества станций по месяцам (для измерений температуры и солёности). Различные столбцы для случаев: (a) все станции, (b) первые десять стандартных станций на разрезе "Кольский меридиан".

![Figure 1](https://github.com/ZifRD/norms_anomalies_TS/blob/master/pics/Figure%201.PNG)

2. Группировка станций по значениям норм 2000 года и новых норм 2020 г. океанографических характеристик методом PCA (для каждого месяца по 4 блока в следующем порядке: старые и новые для температуры, старые и новые нормы солёности); для группировки станций по принадлежности к течениям проведены горизонтальные прямые, вертикальные штриховые прямые для случаев отсутствия данных.

![Figure 2](https://github.com/ZifRD/norms_anomalies_TS/blob/master/pics/Figure%202.PNG)

3. Обеспеченность данными по дням с 1 июля по 1 ноября каждого года с 1970 г. по 2019 г., зелёным цветом показаны 30-дневные фрагменты с центральной точкой - датой начала осени как сезона атмосферной циркуляции по Дзердзеевскому (http://atmosphericcirculation.ru/datas/), вертикальные прямые красного цвета - границы гидрологического предосенья (месяца), определяемые для Баренцева моря (см. Макаревич, П. Р., Дружкова, Е. И. Сезонные циклические процессы в прибрежных планктонных альгоценозах северных морей. - Ростов-на-Дону, 2010. – 280 с.).

![Figure 3](https://github.com/ZifRD/norms_anomalies_TS/blob/master/pics/Figure%203.PNG)

4. Старые (a) и новые (b) нормы температуры (предосенье). 

![Figure 4](https://github.com/ZifRD/norms_anomalies_TS/blob/master/pics/Figure%204.PNG)

5. Периодограммы для 3х течений и различных слоёв: исходные ряды аномалий температуры (a) и первые разности (b); красные вертикальные прямые - границы области спектра с периодами от 3 до 20 лет, а также верхняя граница доверительного интервала (красный шум AR(1), p = 0,95) как ограничения для идентификации значимых пиков.

![Figure 5](https://github.com/ZifRD/norms_anomalies_TS/blob/master/pics/Figure%205.PNG)

6. Матрица взвешенных корреляций (a) и результат восстановления аддитивных компонент (b) ряда аномалий температуры (течение №2, слой “0 м-дно”).

![Figure 6](https://github.com/ZifRD/norms_anomalies_TS/blob/master/pics/Figure%206.PNG)

7. АКФ для отстатка в модели с гармониками с периодами 65, 17, 7.5 и 5.5 лет (a), для варианта с добавленнной компонентой с периодом 3.3 года (b), восстановление компонент (периоды: 5.5 лет, 7.5 лет, 17 лет, 3.3 года).

![Figure 7](https://github.com/ZifRD/norms_anomalies_TS/blob/master/pics/Figure%207.PNG)

<table>
  <tr>
    <td> <img src="Figure 6. Additive model periods.png"  alt="1" width = 360px height = 640px ></td>
    <td><img src="Figure 6. Additive model periods.png" alt="2" width = 360px height = 640px></td>
   </tr> 
   <tr>
      <td><img src="Figure 6. Additive model periods.png" alt="3" width = 360px height = 640px></td>
      <td><img src="Figure 6. Additive model periods.png" alt="4" width = 360px height = 640px></td>
  </tr>
</table>
