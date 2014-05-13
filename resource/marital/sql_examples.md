## SQL query examples - using UN_CrimeRates and UN_Education as an example

### Database schema

	CREATE TABLE "UN_CrimeRates" ("Country" TEXT(255,0),"Year" INTEGER,"Count" INTEGER,"Rate" REAL,"Source" TEXT(255,0),"SourceType" TEXT(255,0));
	CREATE TABLE "UN_Education" ("Country" TEXT(255,0),"Year" INTEGER,"Sex" TEXT(255,0),"Age" TEXT(255,0),"Unit" TEXT(255,0),"Value" INTEGER);

### Notes before starting

- Table names and field names are CASE SENSITIVE
- To view the schema, type `.schema`
- To quit, type `.quit`

### Example queries

#### Selecting all fields and rows from UN_CrimeRates

	SELECT * FROM UN_CrimeRates;

#### Selecting only Country, Year and Count from UN_CrimeRates (aliased as 'cr')

	SELECT cr.Country, cr.Year, cr.Count FROM UN_CrimeRates AS cr;

#### Selecting all fields from UN_CrimeRates in the year 2000

	SELECT * FROM UN_CrimeRates AS cr WHERE cr.Year = 2000;

#### Selecting all fields from UN_CrimeRates in in Thailand

	SELECT * FROM UN_CrimeRates AS cr WHERE cr.Country = 'Thailand';

#### Selecting all fields from UN_CrimeRates in the year 2000, ordered descendingly by rate

	SELECT * FROM UN_CrimeRates AS cr WHERE cr.Year = 2000 ORDER BY cr.Rate DESC;

#### Selecting all fields from UN_CrimeRates in the year 2000, choosing only the top 10 rows, ordered descendingly by rate

	SELECT * FROM UN_CrimeRates AS cr WHERE cr.Year = 2000 ORDER BY cr.Rate DESC LIMIT(10);

#### Selecting Country and the number of record (aliased as 'record_count') for each country in the table

	SELECT cr.Country, COUNT(*) AS record_count FROM UN_CrimeRates AS cr GROUP BY cr.Country;

#### Selecting Country and the total number of crimes recorded for each country in the table, ordered by the highest first

	SELECT cr.Country, SUM(cr.Count) AS total_count FROM UN_CrimeRates AS cr GROUP BY cr.Country ORDER BY total_count DESC;

#### Joining UN_CrimeRates and UN_Education together by year and country (without brackets)

	SELECT * FROM UN_CrimeRates AS cr JOIN UN_Education AS ed ON cr.Year = ed.Year AND cr.Country = ed.Country;

#### Joining UN_CrimeRates and UN_Education together by year and country (with brackets to show the grouping of operations)

	SELECT * FROM (UN_CrimeRates AS cr JOIN UN_Education AS ed ON (cr.Year = ed.Year AND cr.Country = ed.Country));

#### Joining UN_CrimeRates and UN_Education together by year and country, selecting only rows with All genders

	SELECT * FROM (UN_CrimeRates AS cr JOIN UN_Education AS ed ON (cr.Year = ed.Year AND cr.Country = ed.Country)) WHERE ed.Sex = 'All genders';

#### Joining UN_CrimeRates and UN_Education together by year and country, selecting only rows with All genders, showing Country, Year, Crime count and Education count

	SELECT cr.Country, cr.Year, cr.Count, ed.Value FROM (UN_CrimeRates AS cr JOIN UN_Education AS ed ON (cr.Year = ed.Year AND cr.Country = ed.Country)) WHERE ed.Sex = 'All genders';

#### Joining UN_CrimeRates and UN_Education together by year and country, selecting only rows with All genders, showing Country, Year, Crime count, Education count and Education count divided by Crime count, ordered ascendingly by this value

	SELECT cr.Country, cr.Year, cr.Count, ed.Value, (ed.Value/cr.Count) AS edcr FROM (UN_CrimeRates AS cr JOIN UN_Education AS ed ON (cr.Year = ed.Year AND cr.Country = ed.Country)) WHERE ed.Sex = 'All genders' ORDER BY edcr ASC;

#### Joining UN_CrimeRates and UN_Education together by year and country, selecting only rows with All genders, aggregated by Country, showing Country, Crime count, Education count and Education count divided by Crime count, ordered ascendingly by this value

	SELECT cr.Country, SUM(cr.Count), SUM(ed.Value), (SUM(ed.Value)/SUM(cr.Count)) AS edcr FROM (UN_CrimeRates AS cr JOIN UN_Education AS ed ON (cr.Year = ed.Year AND cr.Country = ed.Country)) WHERE ed.Sex = 'All genders' GROUP BY cr.Country ORDER BY edcr ASC;






