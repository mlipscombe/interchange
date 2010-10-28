# Vend::Options::Multi - Interchange Multi product options
#
# $Id: Multi.pm,v 1.00 2007/03/04 10:32:18 jon Exp $
#
# Copyright (C) 2007 Mark Lipscombe <markl@gasupnow.com>
# Copyright (C) 2002-2005 Interchange Development Group <interchange@icdevgroup.org>
# Copyright (C) 2002-2003 Mike Heins <mikeh@perusion.net>

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

package Vend::Options::Multi;

$VERSION = substr(q$Revision: 1.00 $, 10);

=head1 NAME

Vend::Options::Multi - Interchange Multi Options Support

=head1 SYNOPSIS

    [item-options]
 
        or
 
    [price code=SKU]
 
=head1 PREREQUISITES

Vend::Options

=head1 DESCRIPTION

The Vend::Options::Multi module implements a combination of matrix and
regular product options for Interchange.  It is designed to allow a
variant, plus several options available for each of these variants.

If the Interchange Variable MV_OPTION_TABLE is not set, it defaults
to "options", which combines options for Simple, Matrix, and
Modular into that one table. This goes along with foundation and
construct demos up until Interchange 4.9.8.

The "options" table remains the default for matrix options.

=head1 AUTHORS

Mike Heins <mikeh@perusion.net>

=head1 CREDITS

    Jon Jensen <jon@swelter.net>
    Mark Lipscombe <markl@gasupnow.com>

=cut

use Vend::Util;
use Vend::Data;
use Vend::Interpolate;
use Vend::Options;
use strict;

use vars qw/%Default/;

%Default = (
				item_add_routine => 'Vend::Options::Multi::testit',
				table => 'options',
				sort => 'o_sort',
				options_table => 'options',
				variant_table => 'variants',
				variant_sort => 'sort,description',
				option_template => '{LABEL} {PRICE?}({NEGATIVE?}subtract{/NEGATIVE?}{NEGATIVE:}add{/NEGATIVE:} {ABSOLUTE}) {/PRICE?}'
			);

my $Admin_page;

sub price_options { 
	my ($item, $table, $final, $loc) = @_;

	$loc ||= $Vend::Cfg->{Options_repository}{Multi} || {};
	my $map = $loc->{map} || {};

	my $db = database_exists_ref($table || $loc->{options_table} || 'options');
	if(! $db) {
		logOnce('Non-existant price option table %s', $table);
		return;
	}

	my $tname = $db->name();
	my $sku = $item->{mv_sku} || $item->{code};

	my $fsel = $map->{sku} || 'sku';
	my $rsel = $db->quote($sku, $fsel);
	my @rf;
	for(qw/o_group price/) {
		push @rf, ($map->{$_} || $_);
	}

	my $q = "SELECT " . join (",", @rf) . " FROM $tname WHERE $fsel = $rsel AND $rf[1] <> ''";
#::logDebug("Multi module price_options query=$q");
	my $ary = $db->query($q);
	return if ! $ary->[0];
	my $ref;
	my $price = 0;
	my $f;

	foreach $ref (@$ary) {
#::logDebug("checking option " . uneval_it($ref));
		next unless defined $item->{$ref->[0]};
		next unless length($ref->[1]);
		$ref->[1] =~ s/^\s+//;
		$ref->[1] =~ s/\s+$//;
		$ref->[1] =~ s/==/=:/g;
		my %info = split /\s*[=,]\s*/, $ref->[1];
		if(defined $info{ $item->{$ref->[0]} } ) {
			my $atom = $info{ $item->{$ref->[0]} };
			if($atom =~ s/^://) {
				$f = $atom;
				next;
			}
			elsif ($atom =~ s/\%$//) {
				$f = $final if ! defined $f;
				$f += ($atom * $final / 100);
			}
			else {
				$price += $atom;
			}
		}
	}
#::logDebug("price_options returning price=$price f=$f";
	return ($price, $f);
}

sub testit {
	::logDebug("triggered routine testit! args=" . ::uneval(\@_));
}

sub display_options {
	my ($item, $opt, $loc) = @_;

	my @out;
	my $out;

	if($opt->{type} ne 'display') {
		push @out, _display_variants($item, $opt, $loc);
	}
	push @out, _display_options($item, $opt, $loc);

	if($opt->{td}) {
		for(@out) {
			$out .= "<td>$_</td>";
		}
	}
	else {
		$opt->{joiner} = "<br$Vend::Xtrailer>" if ! $opt->{joiner};
		$out = join $opt->{joiner}, @out;
	}

	return $out;
}

sub _display_variants {
	my ($item, $opt, $loc) = @_;

#::logDebug("Multi options by module");
	my $sku = $item->{mv_sku} || $item->{code};
	
	$loc ||= $Vend::Cfg->{Options_repository}{Multi} || \%Default;

	my $map = $loc->{map} || {};

	my $tab = $opt->{variant_table} ||= $loc->{variant_table} || 'variants';
	my $db = database_exists_ref($tab)
			or do {
				logOnce(
						"Multi options: unable to find table %s for item %s",
						$tab,
						$sku,
					);
				return undef;
			};

	my $record;
	if($db->record_exists($sku)) {
		$record = $db->row_hash($sku)
	}
	else {
		$record = {};
		for(qw/display_type/) {
			$record->{$_} = $loc->{$_};
		}
	}

	my $tname = $db->name();

	$opt->{display_type} ||= $record->{display_type};

	$opt->{display_type} = lc $opt->{display_type};
	$opt->{translate} = $loc->{translate} unless defined $opt->{translate};

	my @rf;
	my @out;
	my $out;
	
	# Will be different based on whether separate or not....
	my $rsort;

    my $inv_func;
    if($opt->{inventory}) {
        my ($tab, $col) = split /:+/, $opt->{inventory};
        MAKEFUNC: {
            my $idb = dbref($tab)
                or do {
                    logError("Bad table %s for inventory function.", $tab);
                    last MAKEFUNC;
                };
            $idb->test_column($col)
                or do {
                    logError(
                        "Bad column %s in table %s for inventory function.",
                        $col,
                        $tab,
                    );
                    last MAKEFUNC;
                };
            $inv_func = sub {
                my $key = shift;
                return $idb->field($key, $col);
            };
        }
    }

	use constant SEP_CODE		=> 0;
	use constant SEP_GROUP		=> 1;
	use constant SEP_VALUE		=> 2;
	use constant SEP_LABEL		=> 3;
	use constant SEP_WIDGET		=> 4;
	use constant SEP_PRICE		=> 5;
	use constant SEP_WHOLE		=> 6;
	use constant V_CODE			=> 0;
	use constant V_DESCRIPTION	=> 1;
	use constant V_PRICE			=> 2;

#::logDebug("ready to query options");
	my $vtab = $opt->{variant_table} || $loc->{variant_table};
	my $vdb = database_exists_ref($vtab)
		or do {
			logOnce(
				"Multi options: unable to find variant table %s for item %s",
				$vtab,
				$sku,
			);
			return undef;
		};

	$opt->{type} ||= $record->{widget};
	$rsort = $opt->{variant_sort} || $loc->{variant_sort};
	$rsort = "ORDER BY $rsort" if $rsort;
	$rsort ||= '';

	for(qw/code description price/) {
		push @rf, ($map->{$_} || $_);
	}
	my $lcol = $map->{sku} || 'sku';
	my $lval = $vdb->quote($sku, $lcol);

	my $vname = $vdb->name();

	my $q = "SELECT " . join(",", @rf);
	$q .= " FROM $vname WHERE $lcol = $lval $rsort";
#::logDebug("tag_options multi query: $q");
	my $ary = $vdb->query($q); 
#::logDebug("tag_options multi ary: " . ::uneval($ary));
	my $ref;
	my $price = {};
	foreach $ref (@$ary) {
		# skip unless description
		next unless $ref->[V_DESCRIPTION];

		# skip based on inventory if enabled
		if($inv_func) {
			my $oh = $inv_func->($ref->[V_CODE]);
			next if $oh <= 0;
		}

		my $desc = $ref->[V_DESCRIPTION];
		$desc =~ s/,/&#44;/g;
		$desc =~ s/=/&#61;/g;
		$price->{$ref->[V_CODE]} = $ref->[V_PRICE];
		push @out, "$ref->[V_CODE]=$desc";
	}

	if($opt->{blank_label}) {
		unshift @out, "=$opt->{blank_label}";
	}

	$out .= Vend::Interpolate::tag_accessories(
						$sku,
						'',
						{ 
							attribute => 'code',
							default => undef,
							extra => $opt->{extra},
							item => $item,
							js => $opt->{js},
							name => 'mv_sku',
							passed => join(",", @out),
							price => $opt->{price},
							price_data => $price,
							type => $opt->{type} || 'select',
							display_filter => 'entities',
						},
						$item || undef,
					);
#::logDebug("multi option returning $out");

	return $out;

}

sub _display_options {
	my ($item, $opt, $loc) = @_;

	$loc ||= $Vend::Cfg->{Options_repository}{Multi} || \%Default;
	my $map = $loc->{map} || {};

	my $sku = $item->{mv_sku} || $item->{code};

	my $tab = $opt->{options_table} || $loc->{options_table} || 'options';
	my $db = database_exists_ref($tab)
			or do {
				logOnce(
					"Multi options: unable to find table %s for item %s",
					$tab,
					$sku,
				);
			return undef;
		};
	
	my $tname = $db->name();

	my @rf;
	my @out;
	my $out;

	use constant CODE	=> 0;
	use constant GROUP	=> 1;
	use constant VALUE	=> 2;
	use constant LABEL	=> 3;
	use constant WIDGET	=> 4;
	use constant PRICE	=> 5;
	use constant HEIGHT	=> 6;
	use constant WIDTH	=> 7;

	for(qw/code o_group o_value o_label o_widget price o_height o_width/) {
		push @rf, ($map->{$_} || $_);
	}

	my $fsel = $map->{sku} || 'sku';
	my $rsel = $db->quote($sku, $fsel);

	my $q = "SELECT " . join (",", @rf) . " FROM $tname WHERE $fsel = $rsel";

	if(my $rsort = find_sort($opt, $db, $loc)) {
		$q .= $rsort;
	}
#::logDebug("tag_options simple query: $q");
#
	my $ary = $db->query($q)
		or return;
#::logDebug("tag_options simple ary: " . ::uneval($ary));
#::logDebug("tag_options item=" . ::uneval($item));

	my $ishash = defined $item->{mv_ip} ? 1 : 0;
	my $ref;

	$opt->{option_template} ||= $loc->{option_template};

	foreach $ref (@$ary) {
		# skip unless o_value
		next unless $ref->[VALUE];
#::logDebug("tag_options attribute=" . GROUP);

		if ($opt->{label}) {
			$ref->[LABEL] = "<b>$ref->[LABEL]</b>" if $opt->{bold};
			push @out, $ref->[LABEL];
		}
		my $precursor = $opt->{report}
					? "$ref->[GROUP]$opt->{separator}"
					: qq{<input type="hidden" name="mv_item_option" value="$ref->[GROUP]">};

		my $passed = $ref->[VALUE];
		if($opt->{blank_label}) {
			$passed = "=$opt->{blank_label}, $passed";
		}
		if($opt->{break}) {
			$precursor .= "&nbsp;";
		}
		push @out, $precursor . Vend::Interpolate::tag_accessories(
						$sku,
						'',
						{
							attribute => $ref->[GROUP],
							default => undef,
							extra => $opt->{extra},
							item => $item,
							js => $opt->{js},
							name => $ishash ? undef : "mv_order_$ref->[GROUP]",
							option_template => $opt->{option_template},
							passed => $passed,
							price => $opt->{price},
							price_data => $ref->[PRICE],
							height => $opt->{height} || $ref->[HEIGHT],
							width => $opt->{width} || $ref->[WIDTH],
							type => $opt->{type} || $ref->[WIDGET] || 'select',
							display_filter => 'entities',
						},
						$item || undef,
					);
		if($opt->{break}) {
			push @out, $opt->{break};
		}
	}
	$opt->{joiner} = "<br$Vend::Xtrailer>" if ! $opt->{joiner};
	$out = join $opt->{joiner}, @out;
#::logDebug("display_options out size=" . length($out));
	return $out;
}

sub admin_page {
	my $item = shift;
	my $opt = shift;
	my $page = $Tag->file('include/Options/Multi') || $Admin_page;
	Vend::Util::parse_locale(\$page);
	return interpolate_html($page);
}

$Admin_page = <<'EoAdminPage';
EoAdminPage

1;
