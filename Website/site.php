<?php
if (!defined('INCLUDED_AMEMBER_CONFIG')) 
    die("Direct access to this location is not allowed");


/*function helpdeskEnabled(){
    $user = Am_Di::getInstance()->auth->getUser();
    if(!$user) return false;
    foreach($user->getActiveProducts() as $p){
        $data = $p->getBillingPlanData();
        if($data['helpdesk_enabled']) return true;
    }
    return false;
}
*/
Am_Di::getInstance()->productTable->customFields()
    ->add(new Am_CustomFieldText('renewal_coupon', "Coupon code for renewal"));
Am_Di::getInstance()->productTable->customFields()
    ->add(new Am_CustomFieldText('product_page', "Page with subscription information to point customer to in payment confirmation email"));
	
Am_Di::getInstance()->hook->add(Am_Event::USER_SEARCH_CONDITIONS, 'userSearchCondition');
function userSearchCondition(Am_Event $e){
  $e->addReturn(new Am_Query_User_Condition_NotHaveSubscriptionTo(null, 'none-completed', ___('Having no subscription to:')));
}

Am_Di::getInstance()->hook->add(Am_Event::MAIL_TEMPLATE_BEFORE_PARSE, 'mailTemplateParse');
function mailTemplateParse(Am_Event $e){
    $tmpl = $e->getTemplate();
    $config = $tmpl->getConfig();
	
	if($config['name'] == 'mail_cancel_member') {	
		$product = $tmpl->product;
		$invoice = $tmpl->invoice;
		$access = Am_Di::getInstance()->accessTable->findBy(array('invoice_id' => $invoice->pk()));
		
		$expire_date = date();
		foreach($access as $a){
		  if($a->product_id = $product->pk()) 
			$expire_date = $a->expire_date;
		}    
		$tmpl->setExpiredate(amDate($expire_date));
		$tmpl->setRenewal_coupon($product->data()->get('renewal_coupon'));
	}
	else if($config['name'] == 'send_payment_mail') {			
		$invoice = $tmpl->invoice;
		$access = Am_Di::getInstance()->accessTable->findBy(array('invoice_id' => $invoice->pk()));
		
		foreach($access as $a){
		  $product = Am_Di::getInstance()->productTable->findFirstBy(array('product_id' => $a->product_id));
		  $product_list = $product_list . $product->title . ' : ' . $product->data()->get('product_page') . "\n";
		}    		
		$tmpl->setproduct_list($product_list);
	}
}
						
/*Am_Di::getInstance()->billingPlanTable->customFields()
    ->add(new Am_CustomFieldSelect('helpdesk_enabled', "Enable access to helpdesk", '', '', array('options' => array(0=>'No', 1=>'Yes'))));
 
Am_Di::getInstance()->hook->add('userMenu', 'onUserMenu');
function onUserMenu($event){
    $menu = $event->getMenu();
    if(!helpdeskEnabled()) $menu->removePage($menu->findOneBy('id', 'helpdesk'));  
}
 
Am_Di::getInstance()->hook->add(Am_Event::INIT_CONTROLLER_PAGES, 'onControllerPages');
function onControllerPages($event){
    $c = $event->getController();
    if(!is_a($c, 'Helpdesk_IndexController')) return;
    if(!helpdeskEnabled()) throw new Am_Exception_InputError('Access denied!');
 
}*/
 

/*
Am_Di::getInstance()->hook->add('userMenu', 'siteUserMenu');
function siteUserMenu(Am_Event $event)
{
      // $user = $event->getUser(); // if required
      $menu = $event->getMenu();
 
      // Add a single tab
      $menu->addPage(array(
              'id' => 'mypage',
              'label' => ___('Do The Thing'),
              'uri' => '/the.thing',
              'order' => 2,
       ));
 
       // Add a tab with subitems
       $menu->addPage(
           array(
               'id' => 'with-subitem',
               'label' => 'With Subitems',
               'uri' => '#',
               'pages' => array(
                    array(
                        'id' => 'subitem1',
                        'label' => 'Subitem 1',
                        'uri' => '/subitem-2'
                    ),
                    array(
                        'id' => 'subitem2',
                        'label' => 'Subitem 2',
                        'uri' => '/subitem-2'
                    )
               )
           )
       );
 
       // Remove a tab
       $page = $menu->findOneBy('id', 'add-renew');
       $menu->removePage($page);
 
       // Add sub item to exiting tab
       $page = $menu->findOneBy('id', 'aff'); // get affiliate page 
       $page->addPage(array(
              'id' => 'my-subitem',
              'label' => ___('Subitem'),
              'uri' => '/sub.item',
              'target' => '_blank', // you can set target attribute if necessary
              'order' => 10,
       ));
 
       //Add a tab based on user access, if user has active subscription to product with # 1
	   $products = Am_Di::getInstance()->productCategoryTable->getCategoryProducts();
	   $prodIds = $user->getActiveProductIds();
	   array_intersect($prodIds, $products[$category_id])
       if (in_array(1, $user->getActiveProductIds())) {
       $user = $event->getUser();
           $menu->addPage(array(
              'id' => 'mypage',
              'label' => ___('Do The Thing'),
              'uri' => '/the.thing',
              'order' => 2,
           ));
       }
}
*/
/*<?php 
Am_Di::getInstance()->hook->add(Am_Event::INVOICE_BEFORE_INSERT, 'invoicePublicId');
function invoicePublicId(Am_Event $event){
    $last_invoice_num = Am_Di::getInstance()->store->get('last_invoice_num');
    $invoice = $event->getInvoice();
    //public_id should be unique in database 
    //in case you supply duplicate value here 
    //aMember regenerate it with build in algorithm
    $invoice->public_id = date('Y_m_') . ++$last_invoice_num; //of course you can introduce your own algorithm here
    Am_Di::getInstance()->store->set('last_invoice_num', $last_invoice_num);
}*/