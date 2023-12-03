import part_two


def test_calibration():
    assert 29 == part_two.get_calibration_value("two1nine")
    assert 83 == part_two.get_calibration_value("eightwothree")
    assert 13 == part_two.get_calibration_value("abcone2threexyz")
    assert 24 == part_two.get_calibration_value("xtwone3four")
    assert 42 == part_two.get_calibration_value("4nineeightseven2")
    assert 14 == part_two.get_calibration_value("zoneight234")
    assert 76 == part_two.get_calibration_value("7pqrstsixteen")

    assert 66 == part_two.get_calibration_value("6gqsvsqpzxj")
