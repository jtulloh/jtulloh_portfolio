---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: Predicting the cost of a four night stay in Cape Town
draft: false
image: cape town.jpg
keywords: ""
slug: ipsum
title: Airbnb Regression Analysis
---
*Executive Summary*
In this exercise, we analyse listings from Airbnb bookings (via insideairbnb.com) to better understand price patterns within a particular city. The city we examined was Cape Town and we tried to predict the total cost of four nights for two people
To kick things off, we performed EDA to examine the data in more detail. Next, we subset the data and performed feature engineering to create new features which may suitable for further modeling.
From this exercise, we move to regression analysis where we iteratively test out a few indicators to see if they are significant in explaining the variability within the dataset.
We build up on this to create a final model of 7 covariates (property type, room type, review ratings scores,  cancellation policy,  how many people the property accommodates, which neighbourhood the property is in and if the property has air conditioning installed. This model achieves a **R squared** of **0.512**. Finally, we use this model to make a prediction on unseen problem.