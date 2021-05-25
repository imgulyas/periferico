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

data OrderStatus

-- data CreateOrderRequest

data BillingInfo

data PaymentMethod

data ResponseMeta = RMeta
  { _id :: Int,
    _key :: Text,
    _value :: Text
  }

data LineItem = LineItem
  { _id :: Int,
    _name :: Text,
    _product_id :: Text,
    _quantity :: Int
  }

data ResponseOrder = ROrder
  { _id :: Text,
    _status :: OrderStatus,
    _total :: Int,
    _billing :: BillingInfo,
    _payment_method :: PaymentMethod,
    _payment_method_title :: Text,
    _customer_note :: Text,
    _meta_data :: [ResponseMeta],
    _line_items :: [LineItem]
  }
