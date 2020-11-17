

kaffee <- kaffee %>% 
	reshapePurchasedBrand() %>%     # Create matrix of dummy variables (=1 if brand i was purchased)
	select(-Purchase_Category) %>%  # Variable not in use
	arrange(ID, Days) %>%           # Arrange data by ID and Time
	group_by(ID) %>%
	estimateBrandLoyalty() %>%      # nxp matrix of brand loyalty during first 61 weeks
	estimateInventory() %>%         # Estimate avg. consumption, consumption, and inventory
	lastBrandPurchased() %>%        # Dummy variable (=1 if same brand was purchased on last occasion)
	hasReducedPrice() %>%           # Dummy variable (=1 if price has decreased since last trip)
	averagePurchaseQty() %>%        # Average purchase quantity if something was purchased
	ungroup()
