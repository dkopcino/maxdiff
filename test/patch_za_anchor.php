<html>
<head>
<title>EM TEST</title>
</head>
<body>

<?php

// Dodati u application/helpers/expressions/em_core_helper.php u $this->RDP_ValidFunctions = array(
// 'getanchors' => array('exprmgr_getanchors', 'LEMgetanchors', gT('Get anchors'), 'string getanchors(index, number_of_anchors, best_choice, worst_choice, ... worst_choice)', '', -3)

/**
 * ADDED BY DK: ide u application/helpers/expressions/em_core_helper.php
 * Get anchors.
 * @param array $args, index, number of anchors, best/worst choices
 * @return string
 */
//function exprmgr_getanchors(...$args) // u file na serveru mora ići ovakva deklaracija, ova niže je za testiranje na localhost
function exprmgr_getanchors($args)
{
	$anchors = array();
	$bests = array();
	$worsts = array();
	$cnt = 0;
	$ni = 0;
	$n = 0;
	foreach ($args as $arg) {
		$cnt++;
		if ($cnt == 1) {
			$ni = $arg;
		} else if ($cnt == 2) {
			$n = $arg;
		} else {
			if (($cnt % 2) == 1) {
				// best
				if (array_key_exists($arg, $bests)) {
					$bests[$arg]++;
				} else {
					$bests[$arg] = 1;
				}
			} else {
				// worst
				if (array_key_exists($arg, $worsts)) {
					$worsts[$arg]++;
				} else {
					$worsts[$arg] = 1;
				}
			}
		}
    }
	arsort($bests);
	arsort($worsts);
	$last_added = "worst";
	$i_bests = 0; $i_worsts = 0;
	while (count($anchors) < $n) {
		if ($last_added == "worst") {
			if (count($bests) > $i_bests) {
				$el = (array_slice(array_keys($bests), $i_bests, 1))[0];
				if (!(in_array($el, $anchors))) {
					$anchors[] = $el;
					$last_added = "best";
				}
				$i_bests++;
			} else {
				if (count($worsts) > $i_worsts) {
					$last_added = "best"; // ok, ima ih još za dodati pa nastavljamo s worsts
				} else {
					break;// nema više ni jednih ni drugih, stopiramo
				}
			}
		} else {
			if (count($worsts) > $i_worsts) {
				$el = (array_slice(array_keys($worsts), $i_worsts, 1))[0];
				if (!(in_array($el, $anchors))) {
					$anchors[] = $el;
					$last_added = "worst";
				}
				$i_worsts++;
			} else {
				if (count($bests) > $i_bests) {
					$last_added = "worst"; // ok, ima ih još za dodati pa nastavljamo s bests
				} else {
					break;// nema više ni jednih ni drugih, stopiramo
				}
			}
		}
	}
	
	return $anchors[$ni-1];
}

?>

<script type="text/javascript">

// ADDED BY DK - u /assets/scripts/expressions/em_javascript.js
function LEMgetanchors()
{
	var ni = arguments[0];
	var n = arguments[1];
    anchors = [];
	
	var bests = {};
	var worsts = {};
    for (i = 2; i < arguments.length; ++i) {
        var arg = arguments[i];
		if ((i % 2) == 0) {
			// best
			if (arg in bests) {
				bests[arg]++;
			} else {
				bests[arg] = 1;
			}
		} else {
			// worst
			if (arg in worsts) {
				worsts[arg]++;
			} else {
				worsts[arg] = 1;
			}
		}
    }
	
	var bests_2 = [];
	var worsts_2 = [];
	for (var key in bests) {
        if (bests.hasOwnProperty(key)) {
            bests_2.push({key: key, val: bests[key]});
        }
    }
	for (var key in worsts) {
        if (worsts.hasOwnProperty(key)) {
            worsts_2.push({key: key, val: worsts[key]});
        }
    }
	bests_2 = bests_2.sort(function (a, b) {
      return (b.val - a.val);
	});
	worsts_2 = worsts_2.sort(function (a, b) {
      return (b.val - a.val);
	});
	bests = [];
	worsts = [];
	bests_2.forEach(function(currentValue, index, arr) { bests.push(currentValue.key); });
	worsts_2.forEach(function(currentValue, index, arr) { worsts.push(currentValue.key); });
	var last_added = "worst";
	var i_bests = 0;
	var i_worsts = 0;
	while (anchors.length < n) {
		if (last_added == "worst") {
			if (bests.length > i_bests) {
				el = bests[i_bests];
				if (!(anchors.includes(el))) {
					anchors.push(el);
					last_added = "best";
				}
				i_bests++;
			} else {
				if (worsts.length > i_worsts) {
					last_added = "best"; // ok, ima ih još za dodati pa nastavljamo s worsts
				} else {
					break;// nema više ni jednih ni drugih, stopiramo
				}
			}
		} else {
			if (worsts.length > i_worsts) {
				el = worsts[i_worsts];
				if (!(anchors.includes(el))) {
					anchors.push(el);
					last_added = "worst";
				}
				i_worsts++;
			} else {
				if (bests.length > i_bests) {
					last_added = "worst"; // ok, ima ih još za dodati pa nastavljamo s bests
				} else {
					break;// nema više ni jednih ni drugih, stopiramo
				}
			}
		}
	}
	
	return anchors[ni-1];
}

</script>


BLA
<br/>
<?php echo  
	exprmgr_getanchors(
	3,
	3,
	"miješana", "ribarska", 
	"bolonjez", "ribarska", 
	"miješana", "ribarska", 
	"bolonjez", "ribarska", 
	"miješana", "ribarska",
	"miješana", "bolonjez",
	"ribarska", "bolonjez",
	"miješana", "miješana",
	"ribarska", "miješana",
	"miješana", "ribarska"
	); ?>

<br/>

<p id="demo"></p>

<script type="text/javascript">
var anchors = LEMgetanchors(
	3,
	3,
	"miješana", "bolonjez", 
	"bolonjez", "ribarska", 
	"miješana", "ribarska", 
	"bolonjez", "ribarska", 
	"miješana", "ribarska",
	"miješana", "bolonjez",
	"ribarska", "ribarska",
	"miješana", "miješana",
	"ribarska", "miješana",
	"miješana", "ribarska"
);
document.getElementById("demo").innerHTML = anchors.toString();
</script>

</body>
</html>
