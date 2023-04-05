import pandas as pd
import numpy as np

# customize the path the data before running
food = pd.read_excel('Test Data/FPQ129_CVD04APR2023.xlsx', sheet_name = 0, header = 0, usecols = [i for i in range(25, 154)])

tab = pd.DataFrame()
foods_names = ['Olive Oil', 'Orange Juice', 'Apple Juice', 'Grape Juice', 'Other 100% Juice', 'Other Fruit Drinks', 'Milk', 'Meal Replacement Drink', 'Wine', 'Coffee',
               'Iced Tea', 'Hot Tea', 'Add Sugar', 'Add Artificial Sweetener', 'Add Non-dairy Creamer', 'Add Cream or 50:50', 'Add Milk', 'Cold Cereal', 'Applesauce', 'Apples',
               'Pears', 'Bananas', 'Pineapple', 'Dried Fruit', 'Grapes', 'Mango', 'Papaya', 'Other Fruits', 'Cooked Greens', 'Raw Greens',
               'Carrots', 'String Bean', 'Peas', 'Broccoli', 'Mixed Vegetables', 'Lettuce Salads', 'Sweet Potato', 'French Fries', 'Potato Salad', 'Mashed Potatoes',
               'Salsa or Pico de Gallo', 'Ketchup', 'Chili con Carne', 'Tortillas - Tacos', 'Cooked Dried Beans', 'Avocado', 'Nopal', 'Plaintain', 'Other Vegetables', 'Rice',
               'Pancakes', 'Bagels', 'Whole-grain Breads or Rolls', 'Jam, Jelly, Guava Paste', 'Roast Beef', 'Turkey', 'Deli-luncheon', 'Other Cold Cuts', 'Canned Tuna', 'Beef Hamburger',
               'Ground Beef', 'Hot Dogs', 'Beef Mixtures', 'Steak', 'Ribs', 'Roast Turkey', 'Chicken', 'Baked, Broiled, Fried Chicken', 'Baked Ham', 'Pork',
               'Bacon', 'Sausage', 'Smoked Fish', 'Pizza', 'Crackers', 'Corn Bread', 'Biscuits', 'Tortilla Chips', 'Popcorn', 'Peanuts & Walnuts',
               'Yogurt', 'Cottage Cheese', 'Cheese', 'Frozen Yogurt', 'Ice Cream', 'Pudding', 'Cake', 'Cookies', 'Doughnuts', 'Sweet Muffins',
               'Fruit Crisp', 'Pie', 'Chocolate Candy', 'Other Candy', 'Eggs or Egg Whites', 'Add Sugar-honey to Food', 'Sour Cream', 'Fresh Cream', 'With Added Oils', 'Diet Fruit Drinks',
               'Diet Soft Drinks - Sodas', 'Cooked Cereal', 'Cold Cereal (Whole-grain Type)', 'Salads Made with Dark Green Leaves', 'Corn Tortillas - Tacos', 'Other Cooked Grains', 'White Breads or Rolls', 'Bean Soups', 'Cream Soups', 'Tomato or Vegetable Soups',
               'Pepperoni Pizza', 'Fruit Pie', 'Soft Drink - Sodas', 'Beer', 'Oatmeal', 'Peaches', 'Melons', 'Strawberries', 'Oranges', 'Grapefruit',
               'Corn', 'Fresh Tomatoes', 'Summer Squash', 'Winter Squash', 'Other Fish', 'Soups', 'Corn Oil', 'Canola/rapeseed Oil', 'Other Oil']
foods_names = [foods_names[i] for i in [1,2,3,4,5,99,100,112,8,113,6,7,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,115,116,117,118,119,120,121,122,123,28,29,30,31,32,33,34,103,35,36,37,38,39,40,41,42,44,45,46,47,48,49,105,50,51,52,106,114,101,102,17,104,43,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,124,73,110,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,111,92,93,94,95,96,97,98,107,108,109,125,126,127,128,0]]
tab[''] = foods_names
tab['N'] = np.nan; tab['None'] = np.nan; tab['Monthly'] = np.nan; tab['Weekly'] = np.nan; tab['Daily'] = np.nan; tab['Daily+'] = np.nan; tab['Mode'] = np.nan
for i in range(len(food.columns)):
    col = food[food.columns[i]]
    N = int(sum(np.isnan(col) == False))
    tab['N'][i] = N
    d = int(np.max(col))
    lis = []
    for j in range(d):
        n = np.sum(col == j + 1)
        lis.append(n)
        p = int(round((n/N)*100, 0))
        tab.iat[i, j + 2] = str(p) + '%'
    tab.iat[i, 7] = lis.index(max(lis))

tab.to_csv("s_table_02.csv")