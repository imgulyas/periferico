{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Domain
  ( Order (..),
  )
where

data Order = Order
  { _orderId :: Text,
    _orderDate :: Text,
    _orderStatus :: Text,
    _deliveryDetails :: Text,
    _deliveryStatus :: Text,
    _billingAddress :: Text,
    _shippingAddress :: Text,
    _products :: Text,
    _total :: Text,
    _paymentMethod :: Text,
    _customerNote :: Text,
    _deliveryDeadline :: Text
  }
  deriving stock (Show, Eq)
