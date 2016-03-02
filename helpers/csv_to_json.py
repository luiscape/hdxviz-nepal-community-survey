#!/usr/bin/python
# -*- coding: utf-8 -*-

import os
import csv
import json

dir = os.path.split(os.path.split(os.path.realpath(__file__))[0])[0]

def LoadData(input):
  '''Loading CSV data.'''

  file_path = os.path.join(dir, input)

  try:
    with open(file_path) as csv_file:
      data = csv.DictReader(csv_file)

      csv_input = []
      for row in data:
        csv_input.append(row)

    return csv_input

  except Exception as e:
    print("Could not load CSV.")
    print(e)
    return



def CreateJson(data, output):
  '''JSON creation.'''

  if data is None:
    "No data to transform."
    return

  file_path = os.path.join(dir, output)

  try:
    with open(file_path, 'w') as outfile:
      json.dump(data, outfile, encoding='ISO-8859-1')

    print("JSON file stored successfully.")
    print("JSON PATH: %s" % output)
    return

  except Exception as e:
    print("Could not store JSON file.")
    print(e)
    return

def Main():
  '''Wrapper.'''

  data = LoadData('data/all_rounds_merged.csv')
  CreateJson(data=data, output='http/data/data.json')

if __name__ == '__main__':
  Main()
