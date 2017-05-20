ERBid - Realtime Bidding System
=====

## Table of Content

- [Problem Statement](#problem-statement)
- [Use Cases](#use-cases)
- [Deployment](#deployment)
- [Integration](#integration)
- [Administrative Portal](administrative-portal)

## Problem Statement

Imagine you have a store of a few products, or handy-crafts, or an unsteady supply of items... So you publish these items
on a webpage using Github page with little effort.

Imagine you want to auctionize a few of these items, hoping to sell them at the best price possible. However, you would
have to publish to an auction platform such as eBay.com where you would compete with the unwanted competitors. Besides, you
cannot truly customize the look'n'feel of your given storefront.

However, you do not want to lose control over the look'n'feel of your storefront. You don't want to redirect your potential
customers to another domain.

## Use Cases

### User Registration

Standard flow of user registration
OAuth2 linking is a must-have

### User Login

Standard login flow

### Post new Product Listing

Authenticated user must provide required product details (image URL, product title, description, base price, deadline...) before he can make his product available on the auction platform.

### In Page Auction: Display Auction in Realtime

Using an auction listing id, authorized user can associate a fragment of a webpage to the corresponding auction. The public can see auctioning progress, winning bid and participants in realtime.

### In Page Auction: Place new bid

An authenticated web user can participate in any auction by placing a bid.
