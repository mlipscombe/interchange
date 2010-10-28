# Vend::Payment::Google - Interchange Google Checkout Payments module
#
# Copyright (C) 2007 Telephonyware, LLC
# Copyright (C) 2006 Zolotek Resources Ltd
# All Rights Reserved.
#
# Author: Mark Lipscombe <markl@telephonyware.com>
# Based on the PaypalExpress payment module created by Lyn St George <info@zolotek.net>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA.
#
package Vend::Payment::Google;

=head1 NAME

Vend::Payment::Google - Interchange Google Payments Module




=head1 PREREQUISITES

    Google::Checkout Module Package

=head1 DESCRIPTION

Vend::Payment::Google is a module designed to provide a Google Checkout
payment routine for use within Interchange.

It supports the majority of Google Checkout's features, including:

* Charging and refunding transactions
* Capturing authorized transactions
* Sending shipping & cancel notifications back to Google
* Receiving payment, chargeback and other notifications from Google
* Real-time shipping and sales tax calculation callbacks

=head1 SYNOPSIS

Quick start:

Place this module in <ic_root>/lib/Vend/Payment, and call it in <ic_root>/interchange.cfg with
Require module Vend::Payment::Google. Ensure that your perl installation contains the modules
listed above and their pre-requisites.

This module requires the Google::Checkout and MIME::Base64 modules, both of which are available
on CPAN.

If you use the CGI applications vlink or tlink (as opposed to something like the mod_perl 
Interchange::Link module), you must set up Apache's mod_rewrite in order to have the Authorization
header visible to Interchange.  This is because Apache's mod_cgi does not send any headers
which it should already have processed.  The Authorization header is used to check that it
really is Google who is sending the notification.  To do this, add the following to your Apache
configuration:

RewriteEngine	On
RewriteCond     %{HTTP:Authorization}   ^(.+)
RewriteRule     ^(.*)$ $1 [E=AUTHORIZATION:%1,PT]

Add to catalog.cfg all marked 'required', optionally the others:
Route  google  id xxx  # (required) Your Google Checkout merchant ID
Route  google  secret xxx # (required) Your Google Checkout secret (not the same as your login password)
Route  google  callback https://... # (required) Full URL (using HTTPS) to your Google Checkout callback page
Route  google  host https://sandbox.google.com/checkout/api/checkout/v2/ # (default: sandbox) URL to Google Checkout

Route google_notification_log <<EOF
	empty		1
	encrypt		0
	increment	0
	report		etc/log_google_notification
	supplant	0
	track		logs/google
EOF

To use Google Checkout as well as your existing payment provider, make the following changes:

Add the following to etc/profiles.order:
__NAME__			google
__COMMON_ORDER_PROFILE__
&fatal = yes
email=required
email=email
&set=mv_payment Google
&set=psp Google
&final = yes
&setcheck = payment_method google
__END__

Edit etc/log_transaction, find this section:

[elsif value mv_order_profile eq call]
        [comment] do nothing [/comment]
[/elsif]

And add this immediately below it:

[elsif value mv_order_profile eq google]
        [set card_gateway]google[/set]
        Google Checkout Payment
[/elsif]

Find [elseif variable MV_PAYMENT_MODE], and add [seti card_gateway][var MV_PAYMENT_MODE][/seti] underneath it.

Add into the end of the "[import table=transactions type=LINE continue=NOTES no-commit=1]" section
of etc/log_transaction:
card_gateway: [scratchd card_gateway]
google_protection: 0
google_account_age: 0
google_orderid: [value google_orderid]
google_state: [value google_state]
and add these 4 new columns into your transactions table.

Install etc/log_google_notification, from the same place you got this module, and modify it to suite
your local environment.  By default, it does not log chargeback notifications, which is something you
should care about.  The author of this addon uses a separate payments table to log multiple payments on
a single order, and also to record refunds and chargebacks.

Create a page for processing Google's callbacks.  Google sends callback requests both during the 
checkout process (to calculate shipping to any address, for example), at the conclusion of the 
checkout (to inform of transaction success, etc), and then at any time in the future (to inform
of chargebacks, refunds done through their console, etc).

For example, create a page in pages/google_callback.html:

[charge route="google" google_action="notification"]
[scratchd google_result]

Create Google Checkout buttons on your basket and checkout page.  Be careful of Google's quite
particular requirements about placement of these buttons, and the requirement that a customer
not have to log in or create an account on your site, or specify shipping details on your site.

Your buttons should look like this (be sure to change xxx to your actual merchant ID):

[button
	text="[L]Google Checkout[/L]"
	src="https://checkout.google.com/buttons/checkout.gif?merchant_id=xxx&w=160&h=43&style=white&variant=text&loc=en_US"
	hidetext=1
	form=basket
]
	[charge route="google" google_action="start"]
	[bounce href="[scratch google_redirect]"]
	mv_nextpage=nothing
[/button]



=back

=head1 AUTHORS

Mark Lipscombe <markl@telephonyware.com>
Based on original code by Lyn St George <info@zolotek.net> and Mike Heins <mheins@perusion.com>

=cut

BEGIN {
	eval {
		package Vend::Payment;
		require Google::Checkout::General::GCO or die __PACKAGE__ . " requires Google::Checkout module";

		use Google::Checkout::General::GCO;

		use Google::Checkout::General::MerchantItem;
		use Google::Checkout::General::ShoppingCart;
		use Google::Checkout::XML::CheckoutXmlWriter;
		use Google::Checkout::General::MerchantCheckoutFlow;
		use Google::Checkout::General::ShippingRestrictions;
		use Google::Checkout::General::AddressFilters;
		use Google::Checkout::General::Pickup;
		use Google::Checkout::General::FlatRateShipping;
		use Google::Checkout::General::MerchantCalculatedShipping;
		use Google::Checkout::General::TaxRule;
		use Google::Checkout::General::TaxTable;
		use Google::Checkout::General::TaxTableAreas;
		use Google::Checkout::General::MerchantCalculations;

		use Google::Checkout::Command::AddTrackingData;
		use Google::Checkout::Command::AddMerchantOrderNumber;
		use Google::Checkout::Command::ArchiveOrder;
		use Google::Checkout::Command::CancelOrder;
		use Google::Checkout::Command::ChargeOrder;
		use Google::Checkout::Command::DeliverOrder;
		use Google::Checkout::Command::ProcessOrder;
		use Google::Checkout::Command::RefundOrder;
		use Google::Checkout::Command::SendBuyerMessage;
		use Google::Checkout::Command::UnarchiveOrder;

		use Google::Checkout::Notification::ChargeAmount;
		use Google::Checkout::Notification::ChargebackAmount;
		use Google::Checkout::Notification::NewOrder;
		use Google::Checkout::Notification::Factory qw/get_notification_object/;
		use Google::Checkout::Notification::OrderStateChange;
		use Google::Checkout::Notification::RefundAmount;
		use Google::Checkout::Notification::RiskInformation;

		use Google::Checkout::General::MerchantCalculationCallback;
		use Google::Checkout::General::MerchantCalculationResults;
		use Google::Checkout::General::MerchantCalculationResult;

		use Google::Checkout::XML::Constants;
		use Google::Checkout::General::Util qw/is_gco_error/;
		use Google::Checkout::General::Error;

		use MIME::Base64;
	};

	if ($@) {
		$msg = __PACKAGE__ . ' requires Google::Checkout module';
		::logGlobal ($msg);
		die $msg;
	}

	::logGlobal("%s payment module loaded",__PACKAGE__)
		unless $Vend::Quiet or ! $Global::VendRoot;
}

package Vend::Payment;

sub google {
	my ($user, $amount) = @_;

	my $opt;
	my $secret;

	my %result;
	my $xml_out;

	if(ref $user) {
		$opt = $user;
		$user = $opt->{id} || undef;
		$secret = $opt->{secret} || undef;
	}
	else {
		$opt = {};
	}

	my $actual;

	if($opt->{actual}){
		$actual = $opt->{actual};
	}
	else {
		my (%actual) = map_actual();
		$actual = \%actual;
	}

	my %avs_map = (
		Y => 'Y',
		P => 'Z',
		A => 'A',
		N => 'N',
		U => 'S',
	);

	my $cvn_map = (
		M => 'M',
		N => 'N',
		U => 'U',
		E => 'P',
	);

	my @override = qw/
		order_id
		auth_code
		/;
	for(@override) {
		next unless defined $opt->{$_};
		$actual->{$_} = $opt->{$_};
	}

#::logDebug("actual map result: " . ::uneval($actual));
	if(!$user) {
		$user = charge_param('id')
				or return (
					MStatus => 'failure-hard',
					MErrMsg => errmsg('No account id'),
					);
	}

	$secret = charge_param('secret') if ! $secret;
	my $host = $opt->{host} || charge_param('host') ||
			'https://sandbox.google.com/checkout/api/checkout/v2/';
	
	my $precision = $opt->{precision} || 2;
	my $currency = $opt->{currency} || charge_param('currency') || 
				$::Variable->{MV_PAYMENT_CURRENCY} || 'USD';
	my $amount = $opt->{amount} || Vend::Interpolate::total_cost();
	$amount =~ s/^\D*//g;
	$amount =~ s/\s*//g;
	$amount =~ s/,//g;

	my $possible_us_shipping = $opts->{us_shipping} || $::Variable->{GOOGLE_US_SHIPPING};
	my $possible_intl_shipping = $opts->{intl_shipping} || $::Variable->{GOOGLE_INTL_SHIPPING};

	my $callback = $opt->{callback} || charge_param('callback') ||
				$::Variable->{GOOGLE_SHIPPING_CALLBACK};
	my $edit_url = $opt->{edit_url} || charge_param('edit_url') ||
				Vend::Tags->area({
					href => "ord/basket",
					secure => 1,
				});
	my $shopping_url = $opt->{shopping_url} || charge_param('shopping_url') ||
				Vend::Tags->area({
					href => "index",
					secure => 1,
				});

	my $action = $opt->{google_action} || charge_param('google_action') || '';

	my %type_map = (
		auth			=> 'unsupported',
		authorize		=> 'unsupported',
		sale			=> 'unsupported',
		return			=> 'return',
		mauthreturn		=> 'return',
		settle			=> 'settle',
		settle_prior		=> 'settle',
		void			=> 'void',
		cancel			=> 'void',
		ship			=> 'ship',
	);

	my $transaction = $opt->{transaction} || charge_param('transaction') || '';
	$transaction = $type_map{$transaction};
#::logDebug("action=$action");
	if($action eq "start") {
		my $gco = Google::Checkout::General::GCO->new(
			merchant_id	=> $user,
			merchant_key	=> $secret,
			gco_server	=> $host,
		);
		undef $::Scratch->{google_redirect};
		my $rates = $::Variable->{TAXRATE};
		my $taxable_shipping = $::Variable->{TAXSHIPPING} || '';
		$rates =~ s/^\s+//;
		$rates =~ s/\s+$//;
		my (@rates) = split /\s*,\s*/, $rates;
		my $rate;
		my @tax_rules;
		for(@rates) {
			my($k,$v) = split /\s*=\s*/, $_, 2;
			$area = $k;
			$rate = $v;
			$rate = $rate / 100 if $rate > 1;
			my $tax = Google::Checkout::General::TaxRule->new(
				shipping_tax => $taxable_shipping,
				rate => $rate,
				area => [Google::Checkout::General::TaxTableAreas->new(state => [$k])]);
			push @tax_rules, $tax;
#::logDebug("google tax rate[$k]: " . ::uneval($tax));
		}
#::logDebug("google tax rates: " . ::uneval(\@tax_rules));
		my $tax_table = Google::Checkout::General::TaxTable->new(
			default => 1,
			rules => \@tax_rules);

		my $us_restriction = Google::Checkout::General::ShippingRestrictions->new(
			allowed_country_area => ["ALL"],
			allowed_allow_us_po_box => false,
		);
		my $us_filters = Google::Checkout::General::AddressFilters->new(
			allowed_country_area => ["ALL"],
			allowed_allow_us_po_box => false,
		);
		my $intl_restriction = Google::Checkout::General::ShippingRestrictions->new(
			allowed_world_area => true,
			excluded_country_area => ["ALL"],
		);
		my $intl_filters = Google::Checkout::General::AddressFilters->new(
			allowed_world_area => true,
			excluded_country_area => ["ALL"],
		);

		my @shipping_rules;
		my @methods = split / /, $possible_us_shipping;
		foreach(@methods) {
			my $desc = $Tag->shipping_desc($_);
			my $mco = Google::Checkout::General::MerchantCalculatedShipping->new(
				shipping_name => $desc,
				price => 0.00,
				restriction => $us_restriction,
				address_filters => $us_filters,
			);
			push @shipping_rules, $mco;
		}

		@methods = split / /, $possible_intl_shipping;
		foreach(@methods) {
			my $desc = $Tag->shipping_desc($_);
			my $mco = Google::Checkout::General::MerchantCalculatedShipping->new(
				shipping_name => $desc,
				price => 0.00,
				restriction => $intl_restriction,
				address_filters => $intl_filters,
			);
			push @shipping_rules, $mco;
		}

		my $merchant_calculations = Google::Checkout::General::MerchantCalculations->new(
			url => $callback,
			coupons => 0,
			certificates => 0,
		);

		$order_id  = gen_order_id($opt);

		my $checkout_flow = Google::Checkout::General::MerchantCheckoutFlow->new(
			shipping_method		=> \@shipping_rules,
			edit_cart_url		=> $edit_url,
			continue_shopping_url	=> $shopping_url,
			buyer_phone		=> "1-111-111-1111",
			tax_table		=> [$tax_table],
			merchant_calculation	=> $merchant_calculations,
		);

		my $google_cart = Google::Checkout::General::ShoppingCart->new(
			expiration	=> "+1 month",
			private		=> "$order_id",
			checkout_flow	=> $checkout_flow
		);
#::logDebug("google cart: " . ::uneval($google_cart));

		my $cart = $Vend::Items;
		foreach $item (@$cart) {
			my $code = $item->{code};
			my $mv_ib = $item->{mv_ib};
			my $private = MIME::Base64::encode(::uneval($item), "");

			my $db = Vend::Data::database_exists_ref($mv_ib);
			$db = $db->ref();

			my $google_item = Google::Checkout::General::MerchantItem->new(
				name		=> $db->field($code, "description"),
				description	=> $db->field($code, "subtitle"),
				price		=> Vend::Data::item_price($item, $item->{quantity}, 1),
				quantity	=> $item->{quantity},
				private		=> $private,
			);
			$google_cart->add_item($google_item);
#::logDebug("item: " . ::uneval($google_item));
#::logDebug("item: " . ::uneval($item));
		}

		my $data = $gco->get_xml_and_signature($google_cart);
		my $response = $gco->checkout($google_cart);
::logDebug("data=".::uneval($data));
#::logDebug("respone=".::uneval($response));
		if(is_gco_error($response)) {
			return (
				MStatus => 'failure-hard',
				MErrMsg => errmsg("Unable to contact Google Checkout, please use our regular secure checkout: " . $response->{string}),
			);
		}

		$::Scratch->{google_redirect} = $response;
#::logDebug("response=$response");
		%result = %{$response};
	} elsif($action eq "notification") {
	        my $gco = Google::Checkout::General::GCO->new(
			merchant_id     => $user,
			merchant_key    => $secret,
			gco_server      => $host,
		);

		my $http_ent = ::http()->{entity};
		
		my $in_xml = $opt->{xml} || $$http_ent ||
				return (
					MStatus => 'failure-hard',
					MErrMsg => errmsg("XML must be specified")
				);

#::logDebug("GoogleCheckout: XML=$in_xml");
		# Taken from http://jclark.org/weblog/category/programming/perl/
		my $authed = 0;
		my $env = ::http()->{env};
#::logDebug("env=".uneval($env));
		my $has_auth = (defined($$env{AUTHORIZATION}) and
			(substr($$env{AUTHORIZATION},0,6) eq 'Basic '));
		if($has_auth) {
			my $decoded = decode_base64(substr($$env{AUTHORIZATION},6));
#::logDebug("decoded=$decoded");
			if($decoded =~ /:/) {
				my ($auth_id, $auth_pass) = split(/:/, $decoded);
				if(($auth_id eq $user) and ($auth_pass eq $secret)) {
					$authed = 1;
				}
			} else {
				$has_auth = 0;
			}
		}
		unless($authed) {
::logError("ERROR: Failed authentication on Google callback, check you made the mod_rewrite changes listed in the docs!");
			return (
				MStatus => 'failure-hard',
				MErrMsg => errmsg("Unauthenticated callback request!")
			);
		}
		my $object = get_notification_object(xml => $in_xml);
		$::Values->{google_notification} = $object->type;
#::logDebug("document type: " . $object->type);
		if($object->type eq "new-order-notification") {
			my $serial_number = $object->get_serial_number;
			my $order_number = $object->get_order_number;
			my $time_stamp = $object->get_timestamp;
			my $ful_state = $object->get_fulfillment_state;
			my $fin_state = $object->get_financial_state;
			my $email_allowed = $object->marketing_email_allowed;
			my $calculation_success = $object->merchant_calculation_successful;
			my $total_tax = $object->get_total_tax;
			my $adjust_total = $object->get_adjustment_total;
			my $gc_cal_amount = $object->get_gift_certificate_calculated_amount;
			my $gc_app_amount = $object->get_gift_certificate_applied_amount;
			my $gc_cer_code = $object->get_gift_certificate_code;
			my $gc_message = $object->get_gift_certificate_message;
			my $cu_cal_amount = $object->get_coupon_calculated_amount;
			my $cu_app_amount = $object->get_coupon_applied_amount;
			my $cu_code = $object->get_coupon_code;
			my $cu_message = $object->get_coupon_message;
			my $shipping_name = $object->get_shipping_name || '';
			my $shipping_cost = $object->get_shipping_cost;
			my $cart_expire = $object->get_cart_expiration;
			my $shipping_meth = $object->get_shipping_method;

			# Retrieve items and create a cart
			my $items = $object->get_items();
			for my $item (@$items) {
				my @p = $item->get_private;
				my $private = $p[0][0];
				push @{$Vend::Session->{carts}{main}}, eval(MIME::Base64::decode($private));
			}

			$::Values->{mv_order_route} = "default";
			$::Values->{mv_order_profile} = "google";
			$::Values->{mv_payment} = "Google Checkout";
			$::Values->{google_orderid} = $order_number;
			$::Values->{google_state} = $fin_state;
			$result{'pop.status'} = 'success';
			$result{'order-id'} = $order_number;
			$result{'auth-code'} = $order_number;

			# Set shipping information
			my $buyer_name = get_buyer_info($object,
				Google::Checkout::XML::Constants::GET_SHIPPING,
				Google::Checkout::XML::Constants::BUYER_CONTACT_NAME);
			my @buyer = split / /, $buyer_name;
			my $lname = pop @buyer;
			my $fname = join ' ', @buyer;
			$::Values->{fname} = $fname;
			$::Values->{lname} = $lname;
			$::Values->{company} = get_buyer_info($object,
				Google::Checkout::XML::Constants::GET_SHIPPING,
				Google::Checkout::XML::Constants::BUYER_COMPANY_NAME);
			$::Values->{email} = get_buyer_info($object,
				Google::Checkout::XML::Constants::GET_SHIPPING,
				Google::Checkout::XML::Constants::BUYER_EMAIL);
			$::Values->{phone_day} = get_buyer_info($object,
				Google::Checkout::XML::Constants::GET_SHIPPING,
				Google::Checkout::XML::Constants::BUYER_PHONE) ||
				get_buyer_info($object,
				Google::Checkout::XML::Constants::GET_BILLING,
				Google::Checkout::XML::Constants::BUYER_PHONE);
			$::Values->{fax} = get_buyer_info($object,
				Google::Checkout::XML::Constants::GET_SHIPPING,
				Google::Checkout::XML::Constants::BUYER_FAX);
			$::Values->{address1} = get_buyer_info($object,
				Google::Checkout::XML::Constants::GET_SHIPPING,
				Google::Checkout::XML::Constants::BUYER_ADDRESS1);
			$::Values->{address2} = get_buyer_info($object,
				Google::Checkout::XML::Constants::GET_SHIPPING,
				Google::Checkout::XML::Constants::BUYER_ADDRESS2);
			$::Values->{city} = get_buyer_info($object,
				Google::Checkout::XML::Constants::GET_SHIPPING,
				Google::Checkout::XML::Constants::BUYER_CITY);
			$::Values->{state} = get_buyer_info($object,
				Google::Checkout::XML::Constants::GET_SHIPPING,
				Google::Checkout::XML::Constants::BUYER_REGION);
			$::Values->{zip} = get_buyer_info($object,
				Google::Checkout::XML::Constants::GET_SHIPPING,
				Google::Checkout::XML::Constants::BUYER_POSTAL_CODE);
			$::Values->{country} = get_buyer_info($object,
				Google::Checkout::XML::Constants::GET_SHIPPING,
				Google::Checkout::XML::Constants::BUYER_COUNTRY_CODE);

			# Set billing information
			my $billing_name = get_buyer_info($object,
				Google::Checkout::XML::Constants::GET_BILLING,
				Google::Checkout::XML::Constants::BUYER_CONTACT_NAME);
			my @billing = split / /, $billing_name;
			my $b_lname = pop @billing;
			my $b_fname = join ' ', @billing;
			$::Values->{b_fname} = $b_fname;
			$::Values->{b_lname} = $b_lname;
                        $::Values->{b_company} = get_buyer_info($object,
                                Google::Checkout::XML::Constants::GET_BILLING,
                                Google::Checkout::XML::Constants::BUYER_COMPANY_NAME);
                        $::Values->{b_email} = get_buyer_info($object,
                                Google::Checkout::XML::Constants::GET_BILLING,
                                Google::Checkout::XML::Constants::BUYER_EMAIL);
                        $::Values->{b_phone} = get_buyer_info($object,
                                Google::Checkout::XML::Constants::GET_BILLING,
                                Google::Checkout::XML::Constants::BUYER_PHONE);
                        $::Values->{b_fax} = get_buyer_info($object,
                                Google::Checkout::XML::Constants::GET_BILLING,
                                Google::Checkout::XML::Constants::BUYER_FAX);
                        $::Values->{b_address1} = get_buyer_info($object,
                                Google::Checkout::XML::Constants::GET_BILLING,
                                Google::Checkout::XML::Constants::BUYER_ADDRESS1);
                        $::Values->{b_address2} = get_buyer_info($object,
                                Google::Checkout::XML::Constants::GET_BILLING,
                                Google::Checkout::XML::Constants::BUYER_ADDRESS2);
                        $::Values->{b_city} = get_buyer_info($object,
                                Google::Checkout::XML::Constants::GET_BILLING,
                                Google::Checkout::XML::Constants::BUYER_CITY);
                        $::Values->{b_state} = get_buyer_info($object,
                                Google::Checkout::XML::Constants::GET_BILLING,
                                Google::Checkout::XML::Constants::BUYER_REGION);
                        $::Values->{b_zip} = get_buyer_info($object,
                                Google::Checkout::XML::Constants::GET_BILLING,
                                Google::Checkout::XML::Constants::BUYER_POSTAL_CODE);
                        $::Values->{b_country} = get_buyer_info($object,
                                Google::Checkout::XML::Constants::GET_BILLING,
                                Google::Checkout::XML::Constants::BUYER_COUNTRY_CODE);

			# Work out which shipping method was selected, and ensure
			# it's in the possible options for this country.
			my $possible_shipping = $Tag->shipping("", { possible => 1, });
			my @methods = split / /, $possible_shipping;
			foreach(@methods) {
				my $meth_name = $Tag->shipping_desc($_);
				if($meth_name eq $shipping_name) {
					$::Values->{mv_shipmode} = $_;
				}
			}

			my ($ok, $order_no) = Vend::Order::route_order("main log copy_user",
				$Vend::Session->{carts}{main});

		} elsif($object->type eq "chargeback-amount-notification") {
			my $latest_chargeback_amount = $object->get_latest_chargeback_amount;
			my $total_chargeback_amount = $object->get_total_chargeback_amount;

		} elsif($object->type eq "merchant-calculation-callback") {
			my @calc_results;
			my $items = $object->get_items();
			for my $item (@$items) {
				my @p = $item->get_private;
				my $private = $p[0][0];
				push @{$Vend::Session->{carts}{main}}, eval(MIME::Base64::decode($private));
			}

			my $addresses = $object->get_addresses;
			for my $address (@$addresses) {
				$::Values->{address1} = $address->{id};
				$::Values->{city} = $address->{city};
				$::Values->{state} = $address->{region};
				$::Values->{zip} = $address->{postal_code};
				$::Values->{country} = $address->{country_code};
				my $tax = Vend::Interpolate::salestax();
				my @methods;
				if($::Values->{country} eq "US") {
					@methods = split / /, $possible_us_shipping;
				} else {
					@methods = split / /, $possible_intl_shipping;
				}
				foreach(@methods) {
					my $shippable = 1;
					$cost = $Tag->shipping($_, {noformat => 1});
					unless (defined $cost) {
						$shippable = 0;
					}
#::logDebug("cost=$cost");
					$desc = $Tag->shipping_desc($_);
					my $gc_ship = Google::Checkout::General::MerchantCalculationResult->new(
						shipping_name => $desc,
						address_id => $address->{id},
						shipping_rate => $cost,
						shippable => $shippable,
						total_tax => $tax,
					);
					push @calc_results, $gc_ship;
				}
			}
			$xml_out = Google::Checkout::General::MerchantCalculationResults->new(
				gco => $gco,
				merchant_calculation_result => \@calc_results)->done;
		} elsif($object->type eq "order-state-change-notification") {
			$::Values->{google_orderid} = $object->get_order_number;
			$::Values->{google_new_fill_state} = $object->get_new_fulfillment_order_state;
			$::Values->{google_prev_fill_state} = $object->get_previous_fulfillment_order_state;
			$::Values->{google_new_fin_state} = $object->get_new_financial_order_state;
			$::Values->{google_prev_fin_state} = $object->get_previous_financial_order_state;
			$::Values->{google_reason} = $object->get_reason;
			my ($ok, $order_no) = Vend::Order::route_order("google_notification_log",
				$Vend::Session->{carts}{main});
		} elsif($object->type eq "refund-amount-notification") {
			$::Values->{google_orderid} = $object->get_order_number;
			$::Values->{google_latest_refund_amount} = $object->get_latest_refund_amount;
			$::Values->{google_total_refund_amount} = $object->get_total_refund_amount;
			my ($ok, $order_no) = Vend::Order::route_order("google_notification_log",
				$Vend::Session->{carts}{main});
		} elsif($object->type eq "risk-information-notification") {
			$::Values->{google_orderid} = $object->get_order_number;
			$::Values->{google_avs_response} = $object->get_avs_response;
			$::Values->{google_cvn_response} = $object->get_cvn_response;
			$::Values->{google_partial_cc} = $object->get_partial_cc_number;
			$::Values->{google_protected} = $object->eligible_for_protection;
			$::Values->{google_account_age} = $object->get_buyer_account_age;
			$::Values->{google_ip_address} = $object->get_buyer_ip_address;
			my ($ok, $order_no) = Vend::Order::route_order("google_notification_log",
				$Vend::Session->{carts}{main});
		} elsif($object->type eq "charge-amount-notification") {
			$::Values->{google_orderid} = $object->get_order_number;
			$::Values->{google_latest_charge_amount} = $object->get_latest_charge_amount;
			$::Values->{google_total_charge_amount} = $object->get_total_charge_amount;
			my ($ok, $order_no) = Vend::Order::route_order("google_notification_log",
				$Vend::Session->{carts}{main});
		} else {
			return (
				MStatus => 'failure-hard',
				MErrMsg => errmsg("$object->type is not a supported notification type")
			);
		}

		$::Scratch->{google_result} = $xml_out;
	}

	#
	# Now deal with non-Google specific actions, which are set in $opt->{transaction}.
	# 
	# This gives us some compatibility with other IC payment gateway modules, allowing
	# you to use existing code that settles/refunds/cancels orders.
	#
	if($transaction eq 'settle') {
	        my $gco = Google::Checkout::General::GCO->new(
			merchant_id     => $user,
			merchant_key    => $secret,
			gco_server      => $host,
		);

		my $charge_order = Google::Checkout::Command::ChargeOrder->new(
			order_number => $actual->{order_id},
			amount => $amount);
		my $response = $gco->command($charge_order, 0);
		if(is_gco_error($response)) {
			$result{'pop.status'} = 'failure';
			$result{'pop.error-message'} = errmsg($response->{string});
		} else {
			$result{'pop.status'} = 'success';
			$result{'pop.order-id'} = $actual->{order_id};
			$result{'pop.auth-code'} = $actual->{auth_code};
		}
	} elsif($transaction eq 'return') {
                my $gco = Google::Checkout::General::GCO->new(
                        merchant_id     => $user,
                        merchant_key    => $secret,
                        gco_server      => $host,
                );

		my $refund_order = Google::Checkout::Command::RefundOrder->new(
			order_number => $actual->{order_id},
			amount => $amount,
			comment => $opt->{comment} || "Refund",
			reason => $opt->{reason} || "System generated refund");
		my $response = $gco->command($refund_order, 0);
		if(is_gco_error($response)) {
			$result{'pop.status'} = 'failure';
			$result{'pop.error-message'} = errmsg($response->{string});
		} else {
			$result{'pop.status'} = 'success';
			$result{'pop.order-id'} = $actual->{order_id};
			$result{'pop.auth-code'} = $actual->{auth_code};
			# This is set to allow scripts to differentiate between payment
			# processors like Google that will send a notification for the
			# refund, meaning that we probably should not record a refund
			# until we're notified externally.
			$result{'pop.external-log'} = 1;
		}
	} elsif($transaction eq 'void') {
                my $gco = Google::Checkout::General::GCO->new(
                        merchant_id     => $user,
                        merchant_key    => $secret,
                        gco_server      => $host,
                );

		my $cancel_order = Google::Checkout::Command::CancelOrder->new(
			order_number => $actual->{order_id},
			amount => $amount,
			reason => $opt->{reason} || "System generated cancellation");
		my $response = $gco->command($cancel_order, 0);
		if(is_gco_error($response)) {
			$result{'pop.status'} = 'failure';
			$result{'pop.error-message'} = errmsg($response->{string});
		} else {
			$result{'pop.status'} = 'success';
			$result{'pop.order-id'} = $actual->{order_id};
			$result{'pop.auth-code'} = $actual->{auth_code};
		}
	} elsif($transaction eq 'ship') {
		my $gco = Google::Checkout::General::GCO->new(
			merchant_id	=> $user,
			merchant_key	=> $secret,
			gco_server	=> $host,
		);

		my $tracking_number = $opt->{tracking_number} || charge_param('tracking_number');

		my $add_tracking = Google::Checkout::Command::AddTrackingData->new(
			order_number	=> $actual->{order_id},
			carrier		=> Google::Checkout::XML::Constants::FedeEx,
			tracking_number	=> $tracking_number);
		my $response = $gco->command($add_tracking, 0);
		if(is_gco_error($response)) {
			$result{'pop.status'} = 'failure';
			$result{'pop.error-message'} = errmsg($response->{string});
		} else {
			$result{'pop.status'} = 'success';
			$result{'pop.order-id'} = $actual->{order_id};
		}
	} elsif($transaction eq 'archive') {
		my $gco = Google::Checkout::General::GCO->new(
			merchant_id	=> $user,
			merchant_key	=> $secret,
			gco_server	=> $host,
		);

		my $archive_order = Google::Checkout::Command::ArchiveOrder->new(
			order_number	=> $actual->{order_id});
		my $response = $gco->command($archive_order, 0);
		if(is_gco_error($response)) {
			$result{'pop.status'} = 'failure';
			$result{'pop.error-message'} = errmsg($response->{string});
		} else {
			$result{'pop.status'} = 'success';
			$result{'pop.order-id'} = $actual->{order_id};
		}
	} elsif($transaction eq 'unsupported') {
		$result{'pop.status'} = 'failure';
		$result{'pop.error-message'} = errmsg("Unsupported transaction type $transaction");
	} else {
		# Do nothing for transaction types not explicitly unsupported, for
		# forward compatibility.
	}
	$result{MStatus} = $result{'pop.status'};
	$result{MErrMsg} = $result{'pop.error-message'} if $result{'pop.error-message'};
	$result{'order-id'} = $result{'pop.order-id'} if $result{'pop.order-id'};

	return %result;

}

sub get_buyer_info {
	my ($object, $type, $value) = @_;
	my $r = $object->get_buyer_info($type, $value);
	if(ref $r eq 'HASH') {
		if(scalar %r) {
			return $r;
		} else {
			return undef;
		}
	} elsif(ref $r eq 'ARRAY') {
		if(scalar $r) {
			return $r;
		} else {
			return undef;
		}
	} else {
		return $r;
	}
}

sub run_profile {
	my ($profile, $opt) = @_;

#::logDebug("running profile $profile, order=".$::Values->{google_orderid});
	my $ref = $::Values;

	return 1 if ! $profile;

	my $pname = 'tmp_profile.' . $Vend::Session->{id};
	$profile .= "\n&fatal=1\n";
	$profile = "&noerror=1\n$profile" if $opt->{no_error};
	$profile = "&overwrite=1\n$profile" if $opt->{overwrite_error};
	$::Scratch->{$pname} = $profile;

	my ($status) = ::check_order($pname, $ref);

	delete $::Scratch->{$pname};

	return $status;
}

package Vend::Payment::Google;

1;
