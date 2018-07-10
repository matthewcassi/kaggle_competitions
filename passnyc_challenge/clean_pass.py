def clean_pass(df):

  df['School Income Estimate'] = df['School Income Estimate'].str.replace('$','')
  df['School Income Estimate'] = df['School Income Estimate'].str.replace(',','')
  df['School Income Estimate'] = pd.to_numeric(df['School Income Estimate'])

  cols1 = ['Percent ELL','Percent Asian','Percent Black','Percent Hispanic',
           'Percent Black / Hispanic','Percent White']

  for i in cols1:
    df[i] = df[i].str.replace('%','')
    df[i] = pd.to_numeric(df[i])

  cols2 = ['Student Attendance Rate','Percent of Students Chronically Absent',
           'Rigorous Instruction %','Collaborative Teachers %',
           'Supportive Environment %']

  for i in cols2:
      df[i] = df[i].str.replace('%','')
      df[i] = pd.to_numeric(df[i])

  cols3 = ['Effective School Leadership %','Strong Family-Community Ties %',
           'Trust %']

  for i in cols3:
      df[i] = df[i].str.replace('%','')
      df[i] = pd.to_numeric(df[i])

  return df
