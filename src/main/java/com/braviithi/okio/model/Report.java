package com.braviithi.okio.model;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "Reports")
public class Report implements Serializable {

	private static final long serialVersionUID = -6607362751888781711L;
	
	@Id
    @GeneratedValue(strategy = GenerationType.AUTO)
	@Column(name = "Id")
    private Long id;
	
	@Column(name = "VhMinValue")
	private float vhMinValue;
	@Column(name = "VhMaxValue")
	private float vhMaxValue;
	@Column(name = "HeMinValue")
	private float heMinValue;
	@Column(name = "HeMaxValue")
	private float heMaxValue;
	@Column(name = "HemValue")
	private float hemValue;
	@Column(name = "CwsMinValue")
	private float cwsMinValue;
	@Column(name = "CwsMaxValue")
	private float cwsMaxValue;
	@Column(name = "OrigImageLoc")
	private String origImageLocation;
	@Column(name = "FindingsImageLoc")
	private String findingsImageLocation;
	
	protected Report() {
	}

	public Report(Long id, float vhMinValue, float vhMaxValue, float heMinValue, float heMaxValue, float hemValue,
			float cwsMinValue, float cwsMaxValue, String origImageLocation, String findingsImageLocation) {
		this.id = id;
		this.vhMinValue = vhMinValue;
		this.vhMaxValue = vhMaxValue;
		this.heMinValue = heMinValue;
		this.heMaxValue = heMaxValue;
		this.hemValue = hemValue;
		this.cwsMinValue = cwsMinValue;
		this.cwsMaxValue = cwsMaxValue;
		this.origImageLocation = origImageLocation;
		this.findingsImageLocation = findingsImageLocation;
	}

	
}
