UserTag db-hash Order table column key
UserTag db-hash PosNumber 3
UserTag db-hash addAttr
UserTag db-hash Routine <<EOR
sub {
	my($table, $col, $key, $opt) = @_;
	$col =~ s/:+(.*)//s;
	my $out;
	#$out .= ::uneval(\@_);
	my $rest = $1;
	my $val = ::tag_data($table,$col,$key);
	#$out .= "val=$val";
	my $ref;
	if ($val !~ /\S/) {
		$ref = {};
	}
	else {
		$ref = $Vend::Interpolate::ready_safe->reval($val);
		if (! ref $ref) {
			$ref = {};
		}
	}
	if (! $rest) {
		return $val unless defined $opt->{value};
	}
	my @extra;
	@extra = split /:+/, $rest;
	my $final = pop @extra;
	my $curr = $ref;
	$out .= "Original key request: $rest\n";
	#$out .= ::uneval($ref);
	$out .= "\nFinal key: $final\n";
	for(@extra) {
		$out .= "key --> $_\n";
		$curr = $curr->{$_};
		if (! ref $curr) {
			return "BAD HASH: $out" if $opt->{show_error};
			return;
		}
	}

	if($opt->{keys}) {
		return join get_joiner($opt->{joiner}), sort keys %$curr;
	}
	elsif(! defined $opt->{value}) {
		return $curr->{$final};
	}
	else {
		$curr->{$final} = $opt->{value};
		tag_data($table, $col, $key, { value => ::uneval_it($ref) });
		return $curr->{$final};
	}
}
EOR
