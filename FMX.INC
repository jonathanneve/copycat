	{$IF DECLARED(FireMonkeyVersion)}
	  {$DEFINE HAS_FMX}
	{$ELSE}
	  {$DEFINE HAS_VCL}
	{$IFEND}