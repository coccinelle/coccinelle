int main () {
  err = si476x_core_i2c_xfer(core, SI476X_I2C_RECV,
			     buffer, sizeof(buffer));
  if (err == sizeof(buffer)) return;
}
