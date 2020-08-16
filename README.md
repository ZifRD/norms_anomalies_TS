# norms_anomalies_TS

Материалы для статьи с рабочим названием "ИЗМЕНЧИВОСТЬ АНОМАЛИЙ ТЕМПЕРАТУРЫ И СОЛЕНОСТИ ВОД В АВГУСТЕ-СЕНТЯБРЕ НА РАЗРЕЗЕ «КОЛЬСКИЙ МЕРИДИАН» ЗА ПЕРИОД С 1970 ПО 2019 ГГ." (Моисеев Д.В., Запорожцев И.Ф., Максимовская Т.М., план завершения - сентябрь 2020 г.). Веб-приложение и набор утилит, которые обрабатывают данные World Ocean Database и ICES для обновления референсных значений океанографических характеристик (норм и аномалий) на разрезе "Кольский Меридиан" и построения отчетных изображений для лаб. ОиР ММБИ РАН. 

Входные данные: WOD (World Ocean Database 2018, https://www.nodc.noaa.gov/OC5/SELECT/dbsearch/dbsearch.html), ICES (oceanographic database of the International Council for the Exploration of the Sea, https://www.ices.dk/data/data-portals/Pages/ocean.aspx) и локальные данные ММБИ (25 экспедиций 2000-2019 гг., данные которых отсутствуют в WOD и ICES). Использованы 12579 станций (вертикальных профилей) для температуры и 7023 для солёности. Период: 1970-2019 гг.

Значения WOD извлекаются из ASCII-формата, представленого на портале NOAA, и переформатируются в текстовый табличный формат (CSV со столбцами для месяца, года и строки "ключ-значений" в виде пар "горизонт-измеренная величина". Finally, reconstructed gridded data are obtained (vertical step is 5 m, horizontal step is 0.5 deg. along the Kola Transect). ICES data are in text format too. Local expedition dataset, a set of .xlsx files with common structure (with latitude, longitude, depth and measured values temperature and salinity). ICES and local expedition datasets are treated the same way as WOD files. 
Internal gridded format provides for each cell (2D grid produced by depth-station axes) and each month a pair of ordered lists, i.e. for values of characteristics and for corresponding years. Norm for “cell-month” pair is a mean for the first list, but anomalies calculation require years to be stored (the second list mentioned). Each measured in situ value contributes to only one “cell-month” pair. Length of lists for those pairs varies dramatically depending on station location on the transect (high latitudes are difficult to reach with marine vessels because of ice, etc.), horizon (early measurements are carried out with 50 m step for horizons exceeding 100m), and month (ice cover, etc.). 
External access to calculation is provided with web application implemented by authors in R language with shiny framework. The application is designed to work with the most common input being .csv and .xls/.xlsx file. Use cases includes two general settings on panel to the left (Norms options, Fig. 2): (1) target characteristics (temperature or salinity), (2) time period norms and anomalies to be calculated for. Then user can calculate and export norms to *.xlsx. If user uploads his own in situ measurements (see MMBI local files structure mentioned before) then general dataset is available (option ALL contrary to WOD+ICES, Norms options panel). He also can reduce that dataset to WOD+ICES by clicking Remove previous import. MMBI local files are treated as additional uploaded data to form general dataset (see DB contains block). Panel Anomalies generation has settings of reference month and to variants to calculate anomalies. It can be a table per each year all stacked in one .xlsx file (anomalies are calculated for layers as mentioned in previous section), years selected on the left panel as settings for norms too. Then alternative is to provide some expedition, attribute its data to selected month and obtain one table of anomalies relative to the whole dataset month-specific norms. Output for anomalies in both cases is *.xlsx.


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


