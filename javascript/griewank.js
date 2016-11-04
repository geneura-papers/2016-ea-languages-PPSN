/**
 * @file griewank.js
 * @author Victor Rivas <vrivas@ujaen.es>
 * @date 03/Nov/2016
 * @desc Implementation of function griewank and experiments realated to it for 2016-EvoStar
 */

// Functions have been programmed according to implementation in Go by JJ
// https://github.com/JJ/goEO/blob/master/griewank.go
/**
 * Creates a random chromosome, with values between -600 and 1200
 * @param {Integer} $length Number of genes
 * @returns {Array of integers} $chrom The chromosome
 */
function random_chromosome($length) {
    $chrom = [];
    for ($i = 0; $i < $length; ++$i) {
        $chrom.push(Math.floor(Math.random() * 1200 - 600));
    }
    return $chrom;
}

/**
 * Evaluates a chromosome usign Griewank's function
 * @param {Array of integers} $chrom The chromosome to be evaluated
 * @returns {Float} The value yielded by Griewank's function
 */
function griewank($chrom) {
    $prod = 1;
    $sum = 0;
    for ($i = 0; $i < $chrom.length; ++$i) {
        $value = $chrom[$i];
        $sum += $value * $value / 4000.0;
        $prod *= Math.cos($value / Math.sqrt($i + 1));
    }
    return $sum - $prod + 1;
}

/**
 * Alternative slightly faster implementation
 * @param {type} $chrom
 * @returns {$chrom@call;reduce.prod|Number|$chrom@call;reduce.sum}
 */
function griewank_faster($chrom) {
    var t = $chrom.reduce(function (prev, e, i) {
        return {
            sum: prev.sum += e*e/ 4000
            , prod: prev.prod *= Math.cos(e / Math.sqrt(i + 1))
        };
    }, {sum: 0, prod: 1});
    return t.sum - t.prod + 1;
}

/**
 * Just to test the Griewank function
 * @returns {undefined}
 */
function test() {
    var tests = [
        {"chrom": [1, 2, 3], "result": 1.017028}
        , {"chrom": [0, 0, 0], "result": 0}
        , {"chrom": [-30, -20, 10, 40], "result": 1.750273}
    ];

    tests.forEach(function (e) {
        console.log("Chrom: ", e.chrom
                , " Expected: ", (_e = e.result.toFixed(5))
                , " Yielded: ", (_y = griewank(e.chrom).toFixed(5))
                , " Passed: ", (_e === _y) ? "Yes" : "FAILED!!");
    });
}

/**
 * Executes the experiment using the non-faster implementation of Griewank
 * @returns {undefined}
 */
function main() {
    $ITERATIONS = 100000;
    $DIMENSIONS = [2, 4, 6, 8, 10, 20, 100];

    for (var $i = 0; $i < $DIMENSIONS.length; ++$i) {
        $start = new Date();
        for (var $j = 0; $j < $ITERATIONS; ++$j) {
            griewank(random_chromosome($DIMENSIONS[$i]));
        }
        $end = new Date();
        console.log("JAVASCRIPT-GRIEWANK, ", $DIMENSIONS[$i], ", ", ($end - $start) / 1000);
    }
}

// Execute test() (optional)
if (process.argv.length && process.argv[2] === "--test") test();

// Execute main()
main();


