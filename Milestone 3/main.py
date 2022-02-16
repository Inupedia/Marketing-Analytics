import pandas as pd
import os


def main():
    df = load_data_frame()
    # transform 'year_birth' to age
    transform_dob_to_age(df)
    # transform income to numbers
    transform_income_to_nums(df)
    # replace missing data with average
    fix_missing_data(df)
    # 
    # output
    output_result(df)


def load_data_frame():
    # clean up column names that contain whitespace
    df = pd.read_csv('./raw data/marketing_data.csv')
    df.columns = df.columns.str.replace(' ', '')
    return df


def transform_dob_to_age(data_frame):
    data_frame["Year_Birth"] = 2021 - data_frame["Year_Birth"]
    data_frame.rename(columns={'Year_Birth': 'Age'})


def transform_income_to_nums(data_frame):
    data_frame['Income'] = data_frame['Income'].str.replace(r'\$', '', regex=True)
    data_frame['Income'] = data_frame['Income'].str.replace(r'\,', '', regex=True).astype(float)


def fix_missing_data(data_frame):
    data_frame['Income'] = data_frame['Income'].fillna(data_frame['Income'].median())


def output_result(data_frame):
    os.makedirs('new data', exist_ok=True)
    data_frame.to_csv('./new data/clean_data.csv')


main()
