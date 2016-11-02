<?php
// Functions have been programmed according to implementation in Go by JJ
// https://github.com/JJ/goEO/blob/master/griewank.go
function random_chromosome($length) {
	$chrom=array();
	for ($i= 0; $i  < $length; ++$i ) {
		array_push( $chrom, rand(-600, 600) );
	}
	return $chrom;
}


function griewank( $chrom ) {
   $prod=1;
   $sum=0;
   for( $i=0; $i<count($chrom); ++$i ) {
    $value=$chrom[$i];
    $sum+=$value*$value/4000.0;
    $prod*=cos( $value/sqrt($i+1) );
  }
  return $sum-$prod+1;
}

function test() {
  $chrom=array(30,40,50,60);
  echo "G(1,2,3)=".griewank($chrom);
}
function main() {
    $ITERATIONS = 100000;
    $DIMENSIONS = array(2,4,6,8,10,20,100);
  	srand();
    
    for( $i=0; $i<count($DIMENSIONS); ++$i ) {
        $start=microtime(true);
        for( $j=0; $j<$ITERATIONS; ++$j) {
            griewank( random_chromosome($DIMENSIONS[$i] ));
        }
        $end=microtime(true);
        echo "PHP-GRIEWANK, ".$DIMENSIONS[$i].", ".($end-$start)." secs.\n";
    }
}
main();
?>


