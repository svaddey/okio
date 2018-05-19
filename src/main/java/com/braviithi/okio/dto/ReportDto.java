package com.braviithi.okio.dto;

import org.springframework.web.multipart.MultipartFile;

public class ReportDto {
	private float vhMinValue;
	private float vhMaxValue;
	private float heMinValue;
	private float heMaxValue;
	private float hemValue;
	private float cwsMinValue;
	private float cwsMaxValue;
	private String vhValues;
	private String heValues;
	private String cwsValues;
	private MultipartFile file;
	private String imageUrl;
	private String reportUrl;
	private boolean error;
	private String errorMessage;

	public float getVhMinValue() {
		return vhMinValue;
	}
	public void setVhMinValue(float vhMinValue) {
		this.vhMinValue = vhMinValue;
	}
	public float getVhMaxValue() {
		return vhMaxValue;
	}
	public void setVhMaxValue(float vhMaxValue) {
		this.vhMaxValue = vhMaxValue;
	}
	public float getHeMinValue() {
		return heMinValue;
	}
	public void setHeMinValue(float heMinValue) {
		this.heMinValue = heMinValue;
	}
	public float getHeMaxValue() {
		return heMaxValue;
	}
	public void setHeMaxValue(float heMaxValue) {
		this.heMaxValue = heMaxValue;
	}
	public float getHemValue() {
		return hemValue;
	}
	public void setHemValue(float hemValue) {
		this.hemValue = hemValue;
	}
	public float getCwsMinValue() {
		return cwsMinValue;
	}
	public void setCwsMinValue(float cwsMinValue) {
		this.cwsMinValue = cwsMinValue;
	}
	public float getCwsMaxValue() {
		return cwsMaxValue;
	}
	public void setCwsMaxValue(float cwsMaxValue) {
		this.cwsMaxValue = cwsMaxValue;
	}
	public String getVhValues() {
		return vhValues;
	}
	public void setVhValues(String vhValues) {
		this.vhValues = vhValues;
	}
	public String getHeValues() {
		return heValues;
	}
	public void setHeValues(String heValues) {
		this.heValues = heValues;
	}
	public String getCwsValues() {
		return cwsValues;
	}
	public void setCwsValues(String cwsValues) {
		this.cwsValues = cwsValues;
	}
	public MultipartFile getFile() {
		return file;
	}
	public void setFile(MultipartFile file) {
		this.file = file;
	}
	public String getImageUrl() {
		return imageUrl;
	}
	public void setImageUrl(String imageUrl) {
		this.imageUrl = imageUrl;
	}
	public String getReportUrl() {
		return reportUrl;
	}
	public void setReportUrl(String reportUrl) {
		this.reportUrl = reportUrl;
	}
	public boolean isError() {
		return error;
	}
	public void setError(boolean error) {
		this.error = error;
	}
	public String getErrorMessage() {
		return errorMessage;
	}
	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

}
