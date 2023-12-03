from python_cptanalatriste import part_one_calibration


def test_calibration() -> None:
    assert 12 == part_one_calibration.get_calibration_value("1abc2")
    assert 38 == part_one_calibration.get_calibration_value("pqr3stu8vwx")
    assert 15 == part_one_calibration.get_calibration_value("a1b2c3d4e5f")
    assert 77 == part_one_calibration.get_calibration_value("treb7uchet")
