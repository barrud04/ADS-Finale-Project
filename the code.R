bicycle_sales= read.csv("C:\\Users\\valer\\Desktop\\final r\\bike_sales.csv")

bicycle_sales

library(dplyr)
library(tidyr)
library(ggplot2)

Bikes= bicycle_sales |>
filter(Product_Category== "Bikes")

Bikes= Bikes |>
select(Date, Customer_Age, Customer_Gender, Order_Quantity, Country, State, Sub_Category
, Product_Category)

Bikes= Bikes |>
separate(Date, into = c("Day", "Month", "Year"), sep = "/")

Bikes= Bikes |>
  select(Year, Customer_Age, Customer_Gender, Order_Quantity, Country, Sub_Category)

#טבלה של שנים ומדינות לפי הקנייה
By_country = Bikes |> group_by(Country, Year) |>
  summarise(Sum_Quantity = sum(Order_Quantity)) |> 
  pivot_wider(names_from = Year, values_from = Sum_Quantity)

#טבלה של שנים ומין לפי קניה
by_contry_gender = Bikes |> group_by(Country, Year, Customer_Gender) |>
  summarise(Sum_Quantity = sum(Order_Quantity)) |> 
  pivot_wider(names_from = c(Year, Customer_Gender), values_from = Sum_Quantity)

#אנחנו בודקים כמה בכל מדינה  קנו מכל סוג אופניים לפי מין
by_gender_subcategory = Bikes |> group_by(Country,Customer_Gender, Sub_Category) |>
  summarise(Sum_Quantity = sum(Order_Quantity)) |>
  pivot_wider(names_from = c(Customer_Gender, Sub_Category), values_from = Sum_Quantity, names_sep = "_")



#_________________________סידור מחדש של הטבלה ביי קאנטרי______________________________________#

By_country2= By_country |>
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Purchases") 

#יצירת הגרף

ggplot(By_country2, aes(x = Year, y = Purchases, color = Country, group = Country)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Bike Purchases by Country (2011-2016)",
       x = "Year",
       y = "Number of Purchases") +
  theme_minimal()


#________________________________________________סידור מחדש של הטבלה ביי קאנטרי אנד ג'נדר___________#

by_country_gender2 = by_contry_gender |>
  pivot_longer(cols = -Country, names_to = "Year_Gender", values_to = "Purchases") |>
  separate(Year_Gender, into = c("Year", "Gender"), sep = "_") 

#הגרף

ggplot(by_country_gender2, aes(x = Year, y = Purchases, color = Gender, group = interaction(Country, Gender))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Country) +
  labs(title = "Bike Purchases by Country, Gender, and Year (2011-2016)",
       x = "Year",
       y = "Number of Purchases") +
  theme_minimal()



#______________________________סידור מחדש של הטבלה ביי ג'נדר קאנטרי וסוג אופניים_____________#

by_gender_subcategory2= by_gender_subcategory |>
  pivot_longer(cols = -Country, names_to = "BikeType_Gender", values_to = "Purchases") |>
 
   separate(BikeType_Gender, into = c("Gender", "BikeType"), sep = "_", extra = "merge")
#גרף

ggplot(by_gender_subcategory2, aes(x = BikeType, y = Purchases, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Country) +
  labs(title = "Bike Purchases by Country, Gender, and Bike Type",
       x = "Bike Type",
       y = "Number of Purchases") +
  theme_minimal()


#____________________________________________________________________________________________________#

#על מנת שנוכל לעשות חישובים סטטיסטים בצורה נוחה נצורך להפוך משתנים מסויימים לפקטור: #

By_country2$Country= as.factor(By_country2$Country)
by_country_gender2$Country= as.factor(by_country_gender2$Country)
by_country_gender2$Gender= as.factor(by_country_gender2$Gender)
by_gender_subcategory2$Country= as.factor(by_gender_subcategory2$Country)
by_gender_subcategory2$BikeType= as.factor(by_gender_subcategory2$BikeType)
by_gender_subcategory2$Gender= as.factor(by_gender_subcategory2$Gender)

#__________________________________________________________________________________________________#

#מבחן אף שזה המבחן המקדים לטי להשוואת השונות בין המגדרים ברכישה 

f_test_result = var.test(Purchases ~ Gender, data = by_country_gender2)
print(f_test_result)

#בגלל שהפי וואליו יצא הרבה יותר גדול מ 0.05 אז ממשיכים למבחן טי
# הטי יהיה בלתי תלוי מכיוון שמדובר בגברים ובנשים 

t_test_result= t.test(Purchases ~ Gender, data =  by_country_gender2)
print(t_test_result)

#התשובה שיצאה : אין הבדל משמעותי בממוצעי רכישות האופניים בין גברים לנשים במדגם שלך.#

#________________אנובה דו כיווני -טבלה של סוג אופנים מין ______________#


anova_result = aov(Purchases ~ Gender * BikeType, data = by_gender_subcategory2)
summary(anova_result)



#התשובה: אין הבדל מובהק במספר הרכישות בין גברים לנשים.
יש הבדל מובהק במספר הרכישות בין סוגי האופניים השונים#.
אין אינטראקציה מובהקת בין מגדר לסוג האופניים, כלומר, ההבדלים במספר הרכישות בין סוגי האופניים אינם שונים משמעותית בין גברים לנשים#.

#_______________________________________________________________________טבלה חדשה לחי בריבוע של שכיחות מגדר וסוג___#

table_gender_biketype = table(Bikes$Customer_Gender, Bikes$Sub_Category)

print(table_gender_biketype)

DF_G_T= as.data.frame(table_gender_biketype)

chi_test= chisq.test(table_gender_biketype)
print(chi_test)

#________גרף של הטבלת שכיחויות__________________________#

ggplot(DF_G_T, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Incidence Of Bike Types By Gender",
       x = "Bike Type",
       y = "Incidence Of Bike Types") +
  theme_minimal()

#הוכחנו את מבחן חי בריבוע