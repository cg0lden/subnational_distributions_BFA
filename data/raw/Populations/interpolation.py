import pandas as pd

countries = {'usa': ['1718'],
             'brazil': ['1718'],
             'mexico': ['16'],
             'portugal': ['1516'],
             'bosnia': ['17'],
             'netherlands': ['14', '11', '06'],}
for (country, years) in countries.items():
    for year in years:
        path_in = f'Parsed/Age Ranges/{country}_{year}_range.csv'
        path_out = f'Parsed/Interpolated Age Data/{country}_{year}_interp.csv'
        df_in = pd.read_csv(path_in)
        out = {'age': [], 'pop.male': [], 'pop.female': []}
        for i in range(len(df_in)):
            if i < len(df_in) - 1:
                low_age, high_age = [int(num) for num in df_in['age_range'][i].split('-')]
            else:
                low_age, high_age = 100, 100
            width = high_age - low_age + 1
            pop_male_avg = df_in['pop.male'][i] / width
            pop_female_avg = df_in['pop.female'][i] / width

            for age in range(low_age, high_age + 1):
                out['age'].append(age)
                out['pop.male'].append(pop_male_avg)
                out['pop.female'].append(pop_female_avg)

        df_out = pd.DataFrame(out)
        df_out.to_csv(path_out, index=False)
