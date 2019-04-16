/*********************************************************************************
*  Assignment submission for SQL 												 *
*  Student : Rupa Kadambi  Application ID : APFE18801124						 *
*   																			 *
**********************************************************************************/

/* TASK 1 - UNDERSTANDING THE DATA 
 
A. The collection of tables in the schema represents a database containing information about a business that includes
   the customer information (Cust_dimen table), the products information (prod_dimen table), information about orders 
   fulfilled(market_fact table), the order information(order dimen) and shipping information that indicates the type of
   shipment for an order.
   Using a combination of all these tables, it is possible to gain insights about the business such as the most profitable
   products, the customer ordering the most products, shipping costs, profits etc. It is also possible to slice the data
   by different attributes like region, product category, shipping type, order priority etc. Analysing the data using these
   attributes could help determine the most profitable products and regions and to make informed business decisions based 
   on the results of the analyses.
   The profitability of certain products and regions could help the business to focus marketing and growth efforts in lucrative
   areas with the right products and possibly drop the products/regions that are not profitable.

B. Below is the list of primary and Foreign keys for each table

    TABLE					  PRIMARY KEY		FOREIGN KEY
    cust_dimen				  cust_id			none
    market_fact				  none				ord_id, prod_id, ship_id, cust_id
    orders_dimen			  order_id			ord_id
    prod_dimen				  prod_id			none
    shipping_dimen			  ship_id			order_id
 */

/*-----------------------------------------------------------------------------------------------------------*/

										/*TASK 2 - BASIC ANALYSIS  */

#A. FIND THE TOTAL AND AVERAGE SALES
	
    select sum(sales) as total_sales, avg(sales) as avg_sales
	from market_fact;
 
#B. DISPLAY NO. OF CUSTOMERS IN EACH REGION IN DECREASING ORDER OF CUSTOMERS
	
    select region, count(cust_id) as no_of_customers
	from cust_dimen
	group by region
	order by no_of_customers desc;
 
#C. FIND THE REGION HAVING MAXIMUM CUSTOMERS
    
	select a.region, max(no_of_customers)
    from ( select region, count(cust_id) as no_of_customers
		   from cust_dimen
           group by region
           order by no_of_customers desc) a;
	
    
#D. FIND THE NBR AND ID OF PRODUCTS SOLD IN DECREASING ORDER OF PRODUCTS SOLD

	SELECT prod_id, sum(order_quantity) as no_of_products_sold
	from market_fact
	group by prod_id 
	order by no_of_products_sold desc;

#E. FIND ALL CUSTOMERS FROM ATLANTIC REGION WHO PURCHASED TABLES AND NBR OF TABLES PURCHASED 
    
    select b.customer_name, sum(a.order_quantity) as no_of_tables 
    from
    (
	select m.cust_id, m.order_quantity
    from market_fact m, prod_dimen p
    where (m.prod_id = p.prod_id)
    and p.Product_Sub_Category = 'TABLES') a 
    
    inner join 
    (
     select cust_id, customer_name 
     from cust_dimen c
     where region = 'ATLANTIC'
     ) b
     on a.cust_id = b.cust_id
     group by b.cust_id
    ;

/*-----------------------------------------------------------------------------------------------------------*/

										/*TASK 3 - ADVANCE ANALYSIS  */	

#A. DISPLAY PRODUCT CATEGORIES IN DESCENDING ORDER OF PROFITS

	select p.product_category, sum(m.profit) as profits 
    from market_fact m, prod_dimen p
    where m.prod_id = p.prod_id
    group by p.product_category
    order by profits desc;
    
#B. DISPLAY PROFITS BY PRODUCT CATEGORY AND SUB-CATEGORY

	select p.product_category, p.product_sub_category, sum(m.profit) as total_profit
    from market_fact m, prod_dimen p
    where m.prod_id = p.prod_id
    group by p.product_sub_category
    order by p.product_category, p.product_sub_category;
    
#C. FIND THE LEAST PROFITABLE PRODUCT SUBCATEGORY THAT IS SHIPPED THE MOST

	select b.region, count(b.ship_id) as no_of_shipments, sum(b.profit) as regional_profit
    from
    (
    select p.product_category, p.product_sub_category, sum(m.profit) as total_profit, p.prod_id
    from market_fact m, prod_dimen p
    where m.prod_id = p.prod_id
    group by p.product_sub_category
    order by total_profit asc
    limit 1) a
    
    inner join
    (
    select c.region, m.ship_id, m.profit, m.prod_id 
	from cust_dimen c, market_fact m
	where (c.cust_id = m.cust_id) 
    ) b
    on a.prod_id = b.prod_id
	group by b.region
	order by no_of_shipments desc ;
    
    
    
    
    

