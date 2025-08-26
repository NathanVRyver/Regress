# Regress

[![Haskell](https://img.shields.io/badge/language-Haskell-blue.svg)](https://www.haskell.org/)
[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-green.svg)](LICENSE)

A clean, lightweight Haskell library for linear regression with gradient descent optimization, comprehensive metrics, and CSV data loading capabilities.

## Features

- **Simple API**: Easy-to-use functions for model training and prediction
- **Gradient Descent**: Efficient optimization with customizable learning rate and iterations
- **Comprehensive Metrics**: R², MAE, RMSE, and cross-validation support
- **Data Loading**: Built-in CSV parsing and feature normalization
- **Pure Haskell**: No external dependencies beyond standard libraries

## Installation

```bash
cabal install regress
```

## Quick Start

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Regress

-- Load your data
samples <- loadCSV "data.csv" 

-- Split into training and test sets
let (trainData, testData) = trainTestSplit 0.8 samples

-- Train a model
let model = fit 0.01 10000 trainData  -- learning rate: 0.01, iterations: 10000

-- Make predictions
let prediction = predict model [feature1, feature2, feature3]

-- Evaluate performance
let testR2 = r2Score model testData
let testRmse = rmse model testData
```

## API Reference

### Core Functions

#### `fit :: Double -> Int -> [Sample] -> Model`
Train a linear regression model using gradient descent.
- `Double`: Learning rate
- `Int`: Number of iterations
- `[Sample]`: Training data as (features, target) pairs
- Returns: Trained `Model`

#### `predict :: Model -> Vector -> Double`
Make a prediction using a trained model.
- `Model`: Trained model
- `Vector`: Feature vector (list of doubles)
- Returns: Predicted value

### Metrics

#### `r2Score :: Model -> [Sample] -> Double`
Calculate R-squared (coefficient of determination).

#### `rmse :: Model -> [Sample] -> Double`
Calculate Root Mean Square Error.

#### `mae :: Model -> [Sample] -> Double`
Calculate Mean Absolute Error.

#### `crossValidate :: Int -> Double -> Int -> [Sample] -> Double`
Perform k-fold cross-validation.

### Data Utilities

#### `loadCSV :: FilePath -> IO (Either String [Sample])`
Load data from CSV file. Assumes last column is the target variable.

#### `normalizeFeatures :: [Sample] -> [Sample]`
Normalize features using z-score normalization.

#### `trainTestSplit :: Double -> [Sample] -> ([Sample], [Sample])`
Split data into training and test sets.

