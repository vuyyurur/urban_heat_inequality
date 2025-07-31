import pandas as pd
import requests

# API for Chicago (Illinois: state code 17, Cook County: 031)
url = "https://api.census.gov/data/2020/acs/acs5?get=NAME,B19013_001E&for=tract:*&in=state:17%20county:031"
response = requests.get(url)
data = pd.DataFrame(response.json()[1:], columns=response.json()[0])
data.rename(columns={"B19013_001E": "Median_Income", "tract": "Tract"}, inplace=True)
data.to_csv("data/census_income.csv", index=False)
print("Census data saved to data/census_income.csv")